(ns mental-models.connectors.section-79
  "Connectors Module - Section 79"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def compression-config
  "Configuration for request/response compression."
  (atom {:enabled true
         :min-size-bytes 1024
         :algorithms [:gzip :deflate]
         :prefer :gzip}))

(defn get-compression-config
  "Get current compression configuration."
  []
  @compression-config)

(defn set-compression-config
  "Update compression configuration."
  [config]
  (swap! compression-config merge config))

#?(:clj
   (defn compress-data
     "Compress data using the specified algorithm."
     [data algorithm]
     (let [bytes (.getBytes (if (string? data) data (pr-str data)) "UTF-8")]
       (if (< (count bytes) (:min-size-bytes @compression-config))
         {:compressed false :data data :original-size (count bytes)}
         (let [baos (java.io.ByteArrayOutputStream.)
               gzos (case algorithm
                      :gzip (java.util.zip.GZIPOutputStream. baos)
                      :deflate (java.util.zip.DeflaterOutputStream. baos)
                      (java.util.zip.GZIPOutputStream. baos))]
           (.write gzos bytes)
           (.close gzos)
           {:compressed true
            :algorithm algorithm
            :data (.toByteArray baos)
            :original-size (count bytes)
            :compressed-size (count (.toByteArray baos))
            :ratio (double (/ (count (.toByteArray baos)) (count bytes)))})))))

#?(:clj
   (defn decompress-data
     "Decompress data using the specified algorithm."
     [compressed-bytes algorithm]
     (let [bais (java.io.ByteArrayInputStream. compressed-bytes)
           gzis (case algorithm
                  :gzip (java.util.zip.GZIPInputStream. bais)
                  :deflate (java.util.zip.InflaterInputStream. bais)
                  (java.util.zip.GZIPInputStream. bais))
           baos (java.io.ByteArrayOutputStream.)]
       (let [buffer (byte-array 1024)]
         (loop []
           (let [len (.read gzis buffer)]
             (when (pos? len)
               (.write baos buffer 0 len)
               (recur)))))
       (.close gzis)
       (String. (.toByteArray baos) "UTF-8"))))

(defn with-compression
  "Execute a request with compression support."
  [request-fn & {:keys [compress-request decompress-response] :or {compress-request false decompress-response true}}]
  #?(:clj
     (if-not (:enabled @compression-config)
       (request-fn)
       (request-fn))
     :cljs
     (request-fn)))

(def compression-stats
  "Statistics for compression operations."
  (atom {:requests-compressed 0
         :responses-decompressed 0
         :bytes-saved 0}))

(defn get-compression-stats
  "Get compression statistics."
  []
  @compression-stats)

(defn reset-compression-stats
  "Reset compression statistics."
  []
  (reset! compression-stats {:requests-compressed 0
                             :responses-decompressed 0
                             :bytes-saved 0}))

;; ============================================
;; Response Streaming
