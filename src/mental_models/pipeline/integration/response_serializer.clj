(ns mental-models.pipeline.integration.response-serializer
  "Response serializer for mental model analysis system.
   
   Features:
   - JSON serialization
   - EDN serialization
   - Transit serialization
   - XML serialization
   - Custom serializers
   - Content negotiation
   - Serialization options
   - Serialization metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Date]
           [java.time Instant LocalDate LocalDateTime ZonedDateTime]
           [java.io StringWriter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:serializers {}      ;; content-type -> serializer
         :config {:default-content-type "application/json"
                  :pretty-print? false
                  :include-nil? false
                  :date-format "yyyy-MM-dd'T'HH:mm:ss.SSSZ"}
         :stats {:serializations 0
                 :json-serializations 0
                 :edn-serializations 0
                 :xml-serializations 0
                 :errors 0}
         :initialized? false}))

;; ============================================================================
;; Value Transformers
;; ============================================================================

(defn- transform-value
  "Transform a value for serialization."
  [value]
  (cond
    (instance? UUID value) (str value)
    (instance? Date value) (.toInstant value)
    (instance? Instant value) (str value)
    (instance? LocalDate value) (str value)
    (instance? LocalDateTime value) (str value)
    (instance? ZonedDateTime value) (str value)
    (keyword? value) (name value)
    (symbol? value) (str value)
    :else value))

(defn- transform-map
  "Transform a map for serialization."
  [m]
  (into {}
        (map (fn [[k v]]
               [(if (keyword? k) (name k) (str k))
                (transform-for-json v)])
             m)))

(defn- transform-for-json
  "Transform a value for JSON serialization."
  [value]
  (cond
    (nil? value) nil
    (map? value) (transform-map value)
    (sequential? value) (mapv transform-for-json value)
    (set? value) (mapv transform-for-json value)
    :else (transform-value value)))

;; ============================================================================
;; JSON Serialization
;; ============================================================================

(defn serialize-json
  "Serialize data to JSON."
  [data & {:keys [pretty?] :or {pretty? false}}]
  (swap! state update-in [:stats :json-serializations] inc)
  (let [transformed (transform-for-json data)]
    (if pretty?
      (with-out-str (json/pprint transformed))
      (json/write-str transformed))))

(defn deserialize-json
  "Deserialize JSON to data."
  [json-str & {:keys [keywordize?] :or {keywordize? true}}]
  (json/read-str json-str :key-fn (if keywordize? keyword identity)))

;; ============================================================================
;; EDN Serialization
;; ============================================================================

(defn serialize-edn
  "Serialize data to EDN."
  [data & {:keys [pretty?] :or {pretty? false}}]
  (swap! state update-in [:stats :edn-serializations] inc)
  (if pretty?
    (with-out-str (clojure.pprint/pprint data))
    (pr-str data)))

(defn deserialize-edn
  "Deserialize EDN to data."
  [edn-str]
  (edn/read-string edn-str))

;; ============================================================================
;; XML Serialization
;; ============================================================================

(defn- escape-xml
  "Escape special XML characters."
  [s]
  (when s
    (-> (str s)
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&apos;"))))

(defn- to-xml-element
  "Convert a value to an XML element."
  [tag value indent]
  (let [indent-str (apply str (repeat indent "  "))]
    (cond
      (nil? value)
      (str indent-str "<" (name tag) " xsi:nil=\"true\"/>")
      
      (map? value)
      (str indent-str "<" (name tag) ">\n"
           (str/join "\n" (map (fn [[k v]] (to-xml-element k v (inc indent))) value))
           "\n" indent-str "</" (name tag) ">")
      
      (sequential? value)
      (str/join "\n" (map-indexed (fn [_ v] (to-xml-element tag v indent)) value))
      
      :else
      (str indent-str "<" (name tag) ">" (escape-xml value) "</" (name tag) ">"))))

(defn serialize-xml
  "Serialize data to XML."
  [data & {:keys [root-tag] :or {root-tag :root}}]
  (swap! state update-in [:stats :xml-serializations] inc)
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
       (to-xml-element root-tag data 0)))

;; ============================================================================
;; Custom Serializers
;; ============================================================================

(defn register-serializer!
  "Register a custom serializer."
  [content-type config]
  (let [serializer {:content-type content-type
                    :serialize-fn (get config :serialize-fn)
                    :deserialize-fn (get config :deserialize-fn)
                    :options (get config :options {})
                    :enabled? (atom true)
                    :metrics {:invocations (atom 0)}
                    :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:serializers content-type] serializer)
    (logging/log :info "Registered serializer" {:content-type content-type})
    content-type))

(defn get-serializer
  "Get a serializer by content type."
  [content-type]
  (get-in @state [:serializers content-type]))

(defn list-serializers
  "List all registered serializers."
  []
  (mapv (fn [[ct s]]
          {:content-type ct
           :enabled? @(:enabled? s)
           :invocations @(get-in s [:metrics :invocations])})
        (:serializers @state)))

(defn unregister-serializer!
  "Unregister a serializer."
  [content-type]
  (swap! state update :serializers dissoc content-type))

;; ============================================================================
;; Unified Serialization
;; ============================================================================

(defn serialize
  "Serialize data based on content type."
  [data content-type & opts]
  (swap! state update-in [:stats :serializations] inc)
  
  (try
    (let [ct (or content-type (get-in @state [:config :default-content-type]))]
      (cond
        (str/includes? ct "application/json")
        (apply serialize-json data opts)
        
        (str/includes? ct "application/edn")
        (apply serialize-edn data opts)
        
        (str/includes? ct "application/xml")
        (apply serialize-xml data opts)
        
        (str/includes? ct "text/xml")
        (apply serialize-xml data opts)
        
        :else
        (if-let [serializer (get-serializer ct)]
          (do
            (swap! (get-in serializer [:metrics :invocations]) inc)
            ((:serialize-fn serializer) data))
          (apply serialize-json data opts))))
    (catch Exception e
      (swap! state update-in [:stats :errors] inc)
      (logging/log :error "Serialization error" {:content-type content-type
                                                  :error (.getMessage e)})
      (throw e))))

(defn deserialize
  "Deserialize data based on content type."
  [data-str content-type & opts]
  (let [ct (or content-type (get-in @state [:config :default-content-type]))]
    (cond
      (str/includes? ct "application/json")
      (apply deserialize-json data-str opts)
      
      (str/includes? ct "application/edn")
      (deserialize-edn data-str)
      
      :else
      (if-let [serializer (get-serializer ct)]
        ((:deserialize-fn serializer) data-str)
        (apply deserialize-json data-str opts)))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-serialize-response
  "Ring middleware to serialize response bodies."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)
          content-type (get-in response [:headers "Content-Type"]
                               (get-in @state [:config :default-content-type]))]
      (if (and body (not (string? body)) (not (instance? java.io.InputStream body)))
        (-> response
            (assoc :body (serialize body content-type))
            (assoc-in [:headers "Content-Type"] content-type))
        response))))

(defn wrap-deserialize-request
  "Ring middleware to deserialize request bodies."
  [handler]
  (fn [request]
    (let [body (:body request)
          content-type (get-in request [:headers "content-type"])]
      (if (and body (string? body) content-type)
        (handler (assoc request :body (deserialize body content-type)))
        (handler request)))))

(defn wrap-content-type
  "Ring middleware to set content type based on Accept header."
  [handler]
  (fn [request]
    (let [accept (get-in request [:headers "accept"])
          response (handler request)]
      (if (and accept (not (get-in response [:headers "Content-Type"])))
        (let [content-type (cond
                             (str/includes? accept "application/json") "application/json"
                             (str/includes? accept "application/edn") "application/edn"
                             (str/includes? accept "application/xml") "application/xml"
                             :else (get-in @state [:config :default-content-type]))]
          (assoc-in response [:headers "Content-Type"] content-type))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-content-type!
  "Set the default content type."
  [content-type]
  (swap! state assoc-in [:config :default-content-type] content-type))

(defn set-pretty-print!
  "Enable/disable pretty printing."
  [enabled?]
  (swap! state assoc-in [:config :pretty-print?] enabled?))

(defn set-include-nil!
  "Enable/disable including nil values."
  [enabled?]
  (swap! state assoc-in [:config :include-nil?] enabled?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-serializer-metrics
  "Get serializer metrics."
  []
  (let [stats (:stats @state)]
    {:serializations (:serializations stats)
     :json-serializations (:json-serializations stats)
     :edn-serializations (:edn-serializations stats)
     :xml-serializations (:xml-serializations stats)
     :errors (:errors stats)
     :custom-serializers (count (:serializers @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-serializer-stats
  "Get serializer statistics."
  []
  (merge (get-serializer-metrics)
         {:default-content-type (get-in @state [:config :default-content-type])
          :pretty-print? (get-in @state [:config :pretty-print?])}))

(defn reset-stats!
  "Reset serializer statistics."
  []
  (swap! state assoc :stats {:serializations 0
                             :json-serializations 0
                             :edn-serializations 0
                             :xml-serializations 0
                             :errors 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-serializer!
  "Initialize the response serializer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response serializer initialized")
    (events/emit! :response-serializer-initialized {})
    true))
