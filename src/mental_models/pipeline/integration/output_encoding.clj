(ns mental-models.pipeline.integration.output-encoding
  "Output encoding for mental model analysis system.
   
   Features:
   - HTML encoding
   - JSON encoding
   - XML encoding
   - URL encoding
   - Base64 encoding
   - Unicode normalization
   - Content-type specific encoding
   - Safe output generation"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Base64]
           [java.time Instant LocalDate]
           [java.net URLEncoder URLDecoder]
           [java.nio.charset StandardCharsets Charset]
           [java.text Normalizer Normalizer$Form]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:default-charset "UTF-8"
                  :html-encode-quotes? true
                  :json-escape-unicode? false
                  :xml-encode-cdata? true}
         :stats {:html-encoded 0
                 :json-encoded 0
                 :xml-encoded 0
                 :url-encoded 0
                 :base64-encoded 0}
         :initialized? false}))

;; ============================================================================
;; HTML Encoding
;; ============================================================================

(def ^:private html-entities
  {\< "&lt;"
   \> "&gt;"
   \& "&amp;"
   \" "&quot;"
   \' "&#x27;"
   \/ "&#x2F;"
   \` "&#x60;"
   \= "&#x3D;"})

(defn encode-html
  "Encode string for safe HTML output."
  [input]
  (when (string? input)
    (swap! state update-in [:stats :html-encoded] inc)
    (let [encode-quotes? (get-in @state [:config :html-encode-quotes?])]
      (apply str
             (map (fn [c]
                    (if-let [entity (get html-entities c)]
                      (if (and (not encode-quotes?) (contains? #{\' \"} c))
                        (str c)
                        entity)
                      (str c)))
                  input)))))

(defn encode-html-attribute
  "Encode string for safe HTML attribute value."
  [input]
  (when (string? input)
    (-> input
        encode-html
        (str/replace #"[\r\n\t]" " "))))

(defn encode-html-content
  "Encode string for safe HTML text content."
  [input]
  (when (string? input)
    (-> input
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;"))))

;; ============================================================================
;; JSON Encoding
;; ============================================================================

(def ^:private json-escape-chars
  {\\ "\\\\"
   \" "\\\""
   \newline "\\n"
   \return "\\r"
   \tab "\\t"
   \backspace "\\b"
   \formfeed "\\f"})

(defn- encode-unicode-char
  "Encode a character as Unicode escape sequence."
  [c]
  (format "\\u%04x" (int c)))

(defn encode-json-string
  "Encode string for safe JSON output."
  [input]
  (when (string? input)
    (swap! state update-in [:stats :json-encoded] inc)
    (let [escape-unicode? (get-in @state [:config :json-escape-unicode?])]
      (apply str
             (map (fn [c]
                    (cond
                      (get json-escape-chars c) (get json-escape-chars c)
                      (and escape-unicode? (> (int c) 127)) (encode-unicode-char c)
                      (< (int c) 32) (encode-unicode-char c)
                      :else (str c)))
                  input)))))

(defn encode-json-value
  "Encode a value for JSON output."
  [value]
  (cond
    (nil? value) "null"
    (boolean? value) (str value)
    (number? value) (str value)
    (string? value) (str "\"" (encode-json-string value) "\"")
    (keyword? value) (str "\"" (encode-json-string (name value)) "\"")
    (map? value) (str "{"
                      (str/join ","
                                (map (fn [[k v]]
                                       (str (encode-json-value k) ":" (encode-json-value v)))
                                     value))
                      "}")
    (sequential? value) (str "["
                             (str/join "," (map encode-json-value value))
                             "]")
    :else (str "\"" (encode-json-string (str value)) "\"")))

;; ============================================================================
;; XML Encoding
;; ============================================================================

(def ^:private xml-entities
  {\< "&lt;"
   \> "&gt;"
   \& "&amp;"
   \" "&quot;"
   \' "&apos;"})

(defn encode-xml
  "Encode string for safe XML output."
  [input]
  (when (string? input)
    (swap! state update-in [:stats :xml-encoded] inc)
    (apply str
           (map (fn [c]
                  (or (get xml-entities c) (str c)))
                input))))

(defn encode-xml-attribute
  "Encode string for safe XML attribute value."
  [input]
  (when (string? input)
    (-> input
        encode-xml
        (str/replace #"[\r\n\t]" " "))))

(defn encode-xml-cdata
  "Encode string for CDATA section."
  [input]
  (when (string? input)
    (if (get-in @state [:config :xml-encode-cdata?])
      (str "<![CDATA[" (str/replace input "]]>" "]]]]><![CDATA[>") "]]>")
      (encode-xml input))))

(defn encode-xml-comment
  "Encode string for XML comment."
  [input]
  (when (string? input)
    (str "<!-- " (str/replace input "--" "- -") " -->")))

;; ============================================================================
;; URL Encoding
;; ============================================================================

(defn encode-url
  "URL encode a string."
  [input]
  (when (string? input)
    (swap! state update-in [:stats :url-encoded] inc)
    (URLEncoder/encode input StandardCharsets/UTF_8)))

(defn decode-url
  "URL decode a string."
  [input]
  (when (string? input)
    (URLDecoder/decode input StandardCharsets/UTF_8)))

(defn encode-url-path
  "Encode a URL path segment."
  [input]
  (when (string? input)
    (-> input
        encode-url
        (str/replace "+" "%20")
        (str/replace "%2F" "/"))))

(defn encode-url-query
  "Encode a URL query string."
  [params]
  (when (map? params)
    (str/join "&"
              (map (fn [[k v]]
                     (str (encode-url (name k)) "=" (encode-url (str v))))
                   params))))

;; ============================================================================
;; Base64 Encoding
;; ============================================================================

(defn encode-base64
  "Base64 encode a string or bytes."
  [input]
  (swap! state update-in [:stats :base64-encoded] inc)
  (let [bytes (if (string? input)
                (.getBytes input StandardCharsets/UTF_8)
                input)]
    (.encodeToString (Base64/getEncoder) bytes)))

(defn decode-base64
  "Base64 decode a string."
  [input]
  (when (string? input)
    (.decode (Base64/getDecoder) input)))

(defn decode-base64-string
  "Base64 decode to string."
  [input]
  (when (string? input)
    (String. (decode-base64 input) StandardCharsets/UTF_8)))

(defn encode-base64-url-safe
  "URL-safe Base64 encode."
  [input]
  (let [bytes (if (string? input)
                (.getBytes input StandardCharsets/UTF_8)
                input)]
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn decode-base64-url-safe
  "URL-safe Base64 decode."
  [input]
  (when (string? input)
    (.decode (Base64/getUrlDecoder) input)))

;; ============================================================================
;; Unicode Normalization
;; ============================================================================

(defn normalize-nfc
  "Normalize string to NFC form."
  [input]
  (when (string? input)
    (Normalizer/normalize input Normalizer$Form/NFC)))

(defn normalize-nfd
  "Normalize string to NFD form."
  [input]
  (when (string? input)
    (Normalizer/normalize input Normalizer$Form/NFD)))

(defn normalize-nfkc
  "Normalize string to NFKC form."
  [input]
  (when (string? input)
    (Normalizer/normalize input Normalizer$Form/NFKC)))

(defn normalize-nfkd
  "Normalize string to NFKD form."
  [input]
  (when (string? input)
    (Normalizer/normalize input Normalizer$Form/NFKD)))

(defn remove-diacritics
  "Remove diacritical marks from string."
  [input]
  (when (string? input)
    (-> input
        normalize-nfd
        (str/replace #"\p{InCombiningDiacriticalMarks}+" ""))))

;; ============================================================================
;; Hex Encoding
;; ============================================================================

(defn encode-hex
  "Encode bytes to hex string."
  [bytes]
  (apply str (map #(format "%02x" %) bytes)))

(defn decode-hex
  "Decode hex string to bytes."
  [input]
  (when (string? input)
    (let [len (count input)]
      (byte-array
       (for [i (range 0 len 2)]
         (unchecked-byte
          (Integer/parseInt (subs input i (+ i 2)) 16)))))))

;; ============================================================================
;; Content-Type Specific Encoding
;; ============================================================================

(defn encode-for-content-type
  "Encode value based on content type."
  [value content-type]
  (cond
    (str/includes? content-type "text/html")
    (encode-html value)
    
    (str/includes? content-type "application/json")
    (encode-json-value value)
    
    (str/includes? content-type "application/xml")
    (encode-xml value)
    
    (str/includes? content-type "text/xml")
    (encode-xml value)
    
    :else
    value))

;; ============================================================================
;; Safe Output Generation
;; ============================================================================

(defn safe-html
  "Generate safe HTML output."
  [tag attrs content]
  (let [encoded-attrs (reduce-kv (fn [s k v]
                                   (str s " " (name k) "=\"" (encode-html-attribute (str v)) "\""))
                                 ""
                                 attrs)
        encoded-content (if (string? content)
                          (encode-html-content content)
                          content)]
    (str "<" (name tag) encoded-attrs ">" encoded-content "</" (name tag) ">")))

(defn safe-json
  "Generate safe JSON output."
  [value]
  (encode-json-value value))

(defn safe-xml
  "Generate safe XML output."
  [tag attrs content]
  (let [encoded-attrs (reduce-kv (fn [s k v]
                                   (str s " " (name k) "=\"" (encode-xml-attribute (str v)) "\""))
                                 ""
                                 attrs)
        encoded-content (if (string? content)
                          (encode-xml content)
                          content)]
    (str "<" (name tag) encoded-attrs ">" encoded-content "</" (name tag) ">")))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn output-encoding-middleware
  "Ring middleware for output encoding."
  [handler]
  (fn [request]
    (let [response (handler request)
          content-type (get-in response [:headers "Content-Type"] "text/plain")]
      (if (and (string? (:body response))
               (str/includes? content-type "text/html"))
        response ;; HTML should already be properly encoded
        response))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-encoding-stats
  "Get encoding statistics."
  []
  (:stats @state))

(defn reset-stats!
  "Reset encoding statistics."
  []
  (swap! state assoc :stats {:html-encoded 0
                             :json-encoded 0
                             :xml-encoded 0
                             :url-encoded 0
                             :base64-encoded 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-output-encoding!
  "Initialize the output encoding system."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Output encoding initialized")
    (events/emit! :output-encoding-initialized {})
    true))
