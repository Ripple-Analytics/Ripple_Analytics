(ns mental-models.pipeline.integration.content-negotiator
  "Content negotiator for mental model analysis system.
   
   Features:
   - Accept header parsing
   - Content type negotiation
   - Quality value handling
   - Media type matching
   - Charset negotiation
   - Language negotiation
   - Encoding negotiation
   - Negotiation metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:supported-types {}   ;; type-id -> type-config
         :config {:default-type "application/json"
                  :default-charset "utf-8"
                  :default-language "en"
                  :default-encoding "identity"}
         :stats {:negotiations 0
                 :type-matches 0
                 :type-mismatches 0
                 :fallbacks 0}
         :initialized? false}))

;; ============================================================================
;; Accept Header Parsing
;; ============================================================================

(defn- parse-quality
  "Parse quality value from parameters."
  [params]
  (if-let [q (get params "q")]
    (try (Double/parseDouble q) (catch Exception _ 1.0))
    1.0))

(defn- parse-media-type
  "Parse a single media type with parameters."
  [media-type-str]
  (let [parts (str/split (str/trim media-type-str) #";")
        type-part (str/trim (first parts))
        [type subtype] (str/split type-part #"/")
        params (->> (rest parts)
                    (map str/trim)
                    (map #(str/split % #"="))
                    (filter #(= 2 (count %)))
                    (map (fn [[k v]] [(str/trim k) (str/trim v)]))
                    (into {}))]
    {:type (str/lower-case (or type "*"))
     :subtype (str/lower-case (or subtype "*"))
     :full-type (str/lower-case type-part)
     :quality (parse-quality params)
     :params (dissoc params "q")}))

(defn parse-accept-header
  "Parse an Accept header into a list of media types."
  [accept-header]
  (when accept-header
    (->> (str/split accept-header #",")
         (map parse-media-type)
         (sort-by :quality >)
         (vec))))

(defn parse-accept-charset
  "Parse an Accept-Charset header."
  [header]
  (when header
    (->> (str/split header #",")
         (map (fn [s]
                (let [parts (str/split (str/trim s) #";")
                      charset (str/lower-case (str/trim (first parts)))
                      q (if (> (count parts) 1)
                          (parse-quality {"q" (second (str/split (second parts) #"="))})
                          1.0)]
                  {:charset charset :quality q})))
         (sort-by :quality >)
         (vec))))

(defn parse-accept-language
  "Parse an Accept-Language header."
  [header]
  (when header
    (->> (str/split header #",")
         (map (fn [s]
                (let [parts (str/split (str/trim s) #";")
                      lang (str/lower-case (str/trim (first parts)))
                      q (if (> (count parts) 1)
                          (parse-quality {"q" (second (str/split (second parts) #"="))})
                          1.0)]
                  {:language lang :quality q})))
         (sort-by :quality >)
         (vec))))

(defn parse-accept-encoding
  "Parse an Accept-Encoding header."
  [header]
  (when header
    (->> (str/split header #",")
         (map (fn [s]
                (let [parts (str/split (str/trim s) #";")
                      encoding (str/lower-case (str/trim (first parts)))
                      q (if (> (count parts) 1)
                          (parse-quality {"q" (second (str/split (second parts) #"="))})
                          1.0)]
                  {:encoding encoding :quality q})))
         (sort-by :quality >)
         (vec))))

;; ============================================================================
;; Media Type Matching
;; ============================================================================

(defn- matches-type?
  "Check if a media type matches a pattern."
  [pattern actual]
  (let [pattern-type (:type pattern)
        pattern-subtype (:subtype pattern)
        actual-type (:type actual)
        actual-subtype (:subtype actual)]
    (and (or (= pattern-type "*") (= pattern-type actual-type))
         (or (= pattern-subtype "*") (= pattern-subtype actual-subtype)))))

(defn- specificity
  "Calculate the specificity of a media type pattern."
  [pattern]
  (cond
    (and (= (:type pattern) "*") (= (:subtype pattern) "*")) 0
    (= (:subtype pattern) "*") 1
    :else 2))

;; ============================================================================
;; Supported Types Registration
;; ============================================================================

(defn register-type!
  "Register a supported content type."
  [type-id config]
  (let [type-config {:id type-id
                     :media-type (get config :media-type)
                     :charset (get config :charset "utf-8")
                     :serializer (get config :serializer)
                     :deserializer (get config :deserializer)
                     :priority (get config :priority 100)
                     :enabled? (atom true)
                     :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:supported-types type-id] type-config)
    (logging/log :info "Registered content type" {:type-id type-id :media-type (:media-type type-config)})
    type-id))

(defn get-type
  "Get a registered content type."
  [type-id]
  (get-in @state [:supported-types type-id]))

(defn list-types
  "List all registered content types."
  []
  (mapv (fn [[id t]]
          {:id id
           :media-type (:media-type t)
           :charset (:charset t)
           :enabled? @(:enabled? t)})
        (:supported-types @state)))

(defn unregister-type!
  "Unregister a content type."
  [type-id]
  (swap! state update :supported-types dissoc type-id))

;; ============================================================================
;; Content Negotiation
;; ============================================================================

(defn negotiate-type
  "Negotiate the best content type."
  [accept-header]
  (swap! state update-in [:stats :negotiations] inc)
  
  (let [accepted (or (parse-accept-header accept-header)
                     [{:type "*" :subtype "*" :quality 1.0}])
        supported (->> (vals (:supported-types @state))
                       (filter #(and @(:enabled? %)
                                     (:media-type %)))
                       (map (fn [t]
                              (let [parsed (parse-media-type (:media-type t))]
                                (assoc t :parsed parsed))))
                       (sort-by :priority))]
    
    (if (empty? supported)
      (do
        (swap! state update-in [:stats :fallbacks] inc)
        {:type (get-in @state [:config :default-type])
         :matched? false})
      
      (let [matches (for [accept accepted
                          support supported
                          :when (matches-type? accept (:parsed support))]
                      {:accept accept
                       :support support
                       :score (* (:quality accept)
                                 (+ (specificity accept) 1))})]
        (if-let [best (first (sort-by :score > matches))]
          (do
            (swap! state update-in [:stats :type-matches] inc)
            {:type (:media-type (:support best))
             :charset (:charset (:support best))
             :serializer (:serializer (:support best))
             :matched? true})
          (do
            (swap! state update-in [:stats :type-mismatches] inc)
            {:type (get-in @state [:config :default-type])
             :matched? false}))))))

(defn negotiate-charset
  "Negotiate the best charset."
  [accept-charset-header]
  (let [accepted (or (parse-accept-charset accept-charset-header)
                     [{:charset "*" :quality 1.0}])
        supported #{"utf-8" "iso-8859-1" "us-ascii"}]
    (or (first (filter #(or (= (:charset %) "*")
                            (contains? supported (:charset %)))
                       accepted))
        {:charset (get-in @state [:config :default-charset])})))

(defn negotiate-language
  "Negotiate the best language."
  [accept-language-header]
  (let [accepted (or (parse-accept-language accept-language-header)
                     [{:language "*" :quality 1.0}])
        supported #{"en" "en-us" "en-gb"}]
    (or (first (filter #(or (= (:language %) "*")
                            (contains? supported (:language %)))
                       accepted))
        {:language (get-in @state [:config :default-language])})))

(defn negotiate-encoding
  "Negotiate the best encoding."
  [accept-encoding-header]
  (let [accepted (or (parse-accept-encoding accept-encoding-header)
                     [{:encoding "identity" :quality 1.0}])
        supported #{"gzip" "deflate" "identity"}]
    (or (first (filter #(or (= (:encoding %) "*")
                            (contains? supported (:encoding %)))
                       accepted))
        {:encoding (get-in @state [:config :default-encoding])})))

;; ============================================================================
;; Full Negotiation
;; ============================================================================

(defn negotiate
  "Perform full content negotiation."
  [request]
  (let [accept (get-in request [:headers "accept"])
        accept-charset (get-in request [:headers "accept-charset"])
        accept-language (get-in request [:headers "accept-language"])
        accept-encoding (get-in request [:headers "accept-encoding"])]
    {:content-type (negotiate-type accept)
     :charset (negotiate-charset accept-charset)
     :language (negotiate-language accept-language)
     :encoding (negotiate-encoding accept-encoding)}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-content-negotiation
  "Ring middleware for content negotiation."
  [handler]
  (fn [request]
    (let [negotiated (negotiate request)
          enriched-request (assoc request :negotiated negotiated)
          response (handler enriched-request)]
      (-> response
          (assoc-in [:headers "Content-Type"]
                    (str (get-in negotiated [:content-type :type])
                         "; charset=" (get-in negotiated [:charset :charset])))
          (assoc-in [:headers "Content-Language"]
                    (get-in negotiated [:language :language]))))))

(defn wrap-vary-header
  "Ring middleware to add Vary header."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (assoc-in response [:headers "Vary"]
                "Accept, Accept-Charset, Accept-Language, Accept-Encoding"))))

;; ============================================================================
;; Utilities
;; ============================================================================

(defn accepts?
  "Check if a request accepts a specific media type."
  [request media-type]
  (let [accept (get-in request [:headers "accept"])
        accepted (parse-accept-header accept)
        target (parse-media-type media-type)]
    (some #(matches-type? % target) accepted)))

(defn best-match
  "Find the best matching media type from a list."
  [accept-header available-types]
  (let [accepted (parse-accept-header accept-header)]
    (first (for [accept accepted
                 avail available-types
                 :let [parsed (parse-media-type avail)]
                 :when (matches-type? accept parsed)]
             avail))))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-negotiator-metrics
  "Get negotiator metrics."
  []
  (let [stats (:stats @state)]
    {:negotiations (:negotiations stats)
     :type-matches (:type-matches stats)
     :type-mismatches (:type-mismatches stats)
     :fallbacks (:fallbacks stats)
     :match-rate (if (pos? (:negotiations stats))
                   (/ (:type-matches stats) (:negotiations stats))
                   1.0)
     :supported-types-count (count (:supported-types @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-negotiator-stats
  "Get negotiator statistics."
  []
  (merge (get-negotiator-metrics)
         {:default-type (get-in @state [:config :default-type])
          :default-charset (get-in @state [:config :default-charset])
          :default-language (get-in @state [:config :default-language])}))

(defn reset-stats!
  "Reset negotiator statistics."
  []
  (swap! state assoc :stats {:negotiations 0
                             :type-matches 0
                             :type-mismatches 0
                             :fallbacks 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-content-negotiator!
  "Initialize the content negotiator."
  []
  (when-not (:initialized? @state)
    ;; Register default types
    (register-type! :json
                    {:media-type "application/json"
                     :charset "utf-8"
                     :priority 10})
    
    (register-type! :edn
                    {:media-type "application/edn"
                     :charset "utf-8"
                     :priority 20})
    
    (register-type! :xml
                    {:media-type "application/xml"
                     :charset "utf-8"
                     :priority 30})
    
    (register-type! :html
                    {:media-type "text/html"
                     :charset "utf-8"
                     :priority 40})
    
    (register-type! :text
                    {:media-type "text/plain"
                     :charset "utf-8"
                     :priority 50})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Content negotiator initialized")
    (events/emit! :content-negotiator-initialized {})
    true))
