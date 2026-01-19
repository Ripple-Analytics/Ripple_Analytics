(ns mental-models.pipeline.integration.response-renderer
  "Response renderer for mental model analysis system.
   
   Features:
   - Response rendering
   - Template rendering
   - Content negotiation
   - Format rendering
   - Custom renderers
   - Renderer pipelines
   - Renderer caching
   - Rendering metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:renderers {}        ;; renderer-id -> renderer
         :templates {}        ;; template-id -> template
         :cache {}            ;; cache-key -> rendered content
         :config {:default-format :json
                  :cache-enabled? true
                  :cache-ttl-ms 60000
                  :max-cache-size 1000}
         :stats {:renders 0
                 :cache-hits 0
                 :cache-misses 0
                 :templates-rendered 0}
         :initialized? false}))

;; ============================================================================
;; Built-in Renderers
;; ============================================================================

(defn render-json
  "Render data as JSON."
  [data]
  (letfn [(to-json [v]
            (cond
              (nil? v) "null"
              (boolean? v) (str v)
              (number? v) (str v)
              (string? v) (str "\"" (str/escape v {\" "\\\"" \\ "\\\\" \newline "\\n" \return "\\r" \tab "\\t"}) "\"")
              (keyword? v) (str "\"" (name v) "\"")
              (map? v) (str "{"
                            (str/join "," (map (fn [[k val]]
                                                 (str (to-json (if (keyword? k) (name k) (str k)))
                                                      ":" (to-json val)))
                                               v))
                            "}")
              (sequential? v) (str "[" (str/join "," (map to-json v)) "]")
              :else (str "\"" (str v) "\"")))]
    (to-json data)))

(defn render-edn
  "Render data as EDN."
  [data]
  (pr-str data))

(defn render-xml
  "Render data as XML."
  [data & {:keys [root-tag] :or {root-tag "root"}}]
  (letfn [(to-xml [v tag]
            (cond
              (nil? v) (str "<" tag "/>")
              (map? v) (str "<" tag ">"
                            (str/join "" (map (fn [[k val]]
                                                (to-xml val (name k)))
                                              v))
                            "</" tag ">")
              (sequential? v) (str/join "" (map-indexed (fn [i item]
                                                          (to-xml item "item"))
                                                        v))
              :else (str "<" tag ">" (str v) "</" tag ">")))]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
         (to-xml data root-tag))))

(defn render-csv
  "Render data as CSV."
  [data]
  (when (sequential? data)
    (let [headers (if (map? (first data))
                    (keys (first data))
                    (range (count (first data))))
          escape-csv (fn [v]
                       (let [s (str v)]
                         (if (or (str/includes? s ",")
                                 (str/includes? s "\"")
                                 (str/includes? s "\n"))
                           (str "\"" (str/replace s "\"" "\"\"") "\"")
                           s)))]
      (str (str/join "," (map name headers)) "\n"
           (str/join "\n"
                     (map (fn [row]
                            (str/join ","
                                      (map (fn [h]
                                             (escape-csv (get row h)))
                                           headers)))
                          data))))))

(defn render-html
  "Render data as HTML."
  [data & {:keys [title] :or {title "Response"}}]
  (letfn [(to-html [v depth]
            (cond
              (nil? v) "<span class=\"null\">null</span>"
              (boolean? v) (str "<span class=\"boolean\">" v "</span>")
              (number? v) (str "<span class=\"number\">" v "</span>")
              (string? v) (str "<span class=\"string\">\"" v "\"</span>")
              (keyword? v) (str "<span class=\"keyword\">" (name v) "</span>")
              (map? v) (str "<div class=\"object\" style=\"margin-left:" (* depth 20) "px\">"
                            (str/join ""
                                      (map (fn [[k val]]
                                             (str "<div class=\"property\">"
                                                  "<span class=\"key\">" (name k) ":</span> "
                                                  (to-html val (inc depth))
                                                  "</div>"))
                                           v))
                            "</div>")
              (sequential? v) (str "<div class=\"array\" style=\"margin-left:" (* depth 20) "px\">"
                                   (str/join ""
                                             (map-indexed (fn [i item]
                                                            (str "<div class=\"item\">[" i "] "
                                                                 (to-html item (inc depth))
                                                                 "</div>"))
                                                          v))
                                   "</div>")
              :else (str "<span>" v "</span>")))]
    (str "<!DOCTYPE html><html><head><title>" title "</title>"
         "<style>"
         "body { font-family: monospace; padding: 20px; }"
         ".null { color: #999; }"
         ".boolean { color: #0000ff; }"
         ".number { color: #009900; }"
         ".string { color: #990000; }"
         ".keyword { color: #990099; }"
         ".key { font-weight: bold; }"
         "</style></head><body>"
         (to-html data 0)
         "</body></html>")))

(defn render-text
  "Render data as plain text."
  [data]
  (with-out-str (clojure.pprint/pprint data)))

(def built-in-renderers
  {:json {:render-fn render-json
          :content-type "application/json"}
   :edn {:render-fn render-edn
         :content-type "application/edn"}
   :xml {:render-fn render-xml
         :content-type "application/xml"}
   :csv {:render-fn render-csv
         :content-type "text/csv"}
   :html {:render-fn render-html
          :content-type "text/html"}
   :text {:render-fn render-text
          :content-type "text/plain"}})

;; ============================================================================
;; Custom Renderers
;; ============================================================================

(defn register-renderer!
  "Register a custom renderer."
  [renderer-id config]
  (let [renderer {:id renderer-id
                  :name (get config :name (name renderer-id))
                  :render-fn (get config :render-fn)
                  :content-type (get config :content-type "application/octet-stream")
                  :enabled? (atom true)
                  :metrics {:renders (atom 0)}
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:renderers renderer-id] renderer)
    (logging/log :info "Registered renderer" {:renderer-id renderer-id})
    renderer-id))

(defn get-renderer
  "Get a renderer."
  [renderer-id]
  (or (get-in @state [:renderers renderer-id])
      (get built-in-renderers renderer-id)))

(defn list-renderers
  "List all renderers."
  []
  (concat
   (mapv (fn [[id r]] {:id id :type :built-in :content-type (:content-type r)})
         built-in-renderers)
   (mapv (fn [[id r]]
           {:id id
            :name (:name r)
            :type :custom
            :content-type (:content-type r)
            :enabled? @(:enabled? r)})
         (:renderers @state))))

;; ============================================================================
;; Template Management
;; ============================================================================

(defn register-template!
  "Register a template."
  [template-id config]
  (let [template {:id template-id
                  :name (get config :name (name template-id))
                  :content (get config :content)
                  :variables (get config :variables [])
                  :format (get config :format :text)
                  :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:templates template-id] template)
    template-id))

(defn get-template
  "Get a template."
  [template-id]
  (get-in @state [:templates template-id]))

(defn render-template
  "Render a template with variables."
  [template-id variables]
  (when-let [template (get-template template-id)]
    (swap! state update-in [:stats :templates-rendered] inc)
    (reduce (fn [content [k v]]
              (str/replace content (str "{{" (name k) "}}") (str v)))
            (:content template)
            variables)))

;; ============================================================================
;; Content Negotiation
;; ============================================================================

(defn parse-accept-header
  "Parse Accept header into format preferences."
  [accept-header]
  (when accept-header
    (->> (str/split accept-header #",")
         (map str/trim)
         (map (fn [part]
                (let [[media-type & params] (str/split part #";")
                      q (or (some (fn [p]
                                    (when (str/starts-with? (str/trim p) "q=")
                                      (Double/parseDouble (subs (str/trim p) 2))))
                                  params)
                            1.0)]
                  {:media-type (str/trim media-type) :q q})))
         (sort-by :q >))))

(defn negotiate-format
  "Negotiate response format based on Accept header."
  [accept-header]
  (let [preferences (parse-accept-header accept-header)
        format-map {"application/json" :json
                    "application/edn" :edn
                    "application/xml" :xml
                    "text/xml" :xml
                    "text/csv" :csv
                    "text/html" :html
                    "text/plain" :text
                    "*/*" (get-in @state [:config :default-format])}]
    (or (some (fn [{:keys [media-type]}]
                (get format-map media-type))
              preferences)
        (get-in @state [:config :default-format]))))

;; ============================================================================
;; Rendering
;; ============================================================================

(defn render
  "Render data in specified format."
  [data format]
  (swap! state update-in [:stats :renders] inc)
  
  (if-let [renderer (get-renderer format)]
    (let [render-fn (or (:render-fn renderer)
                        (get-in built-in-renderers [format :render-fn]))]
      (when render-fn
        (when-let [custom-renderer (get-in @state [:renderers format])]
          (swap! (get-in custom-renderer [:metrics :renders]) inc))
        (render-fn data)))
    (render-json data)))

(defn render-response
  "Render a complete response."
  [data format]
  (let [renderer (get-renderer format)
        content-type (or (:content-type renderer)
                         (get-in built-in-renderers [format :content-type])
                         "application/json")
        body (render data format)]
    {:status 200
     :headers {"Content-Type" content-type}
     :body body}))

;; ============================================================================
;; Caching
;; ============================================================================

(defn- cache-key
  "Generate a cache key."
  [data format]
  (str (hash data) "-" format))

(defn- get-cached
  "Get cached rendered content."
  [key]
  (when (get-in @state [:config :cache-enabled?])
    (when-let [entry (get-in @state [:cache key])]
      (when (< (- (System/currentTimeMillis) (:timestamp entry))
               (get-in @state [:config :cache-ttl-ms]))
        (swap! state update-in [:stats :cache-hits] inc)
        (:content entry)))))

(defn- put-cached!
  "Put rendered content in cache."
  [key content]
  (when (get-in @state [:config :cache-enabled?])
    (let [max-size (get-in @state [:config :max-cache-size])]
      (swap! state update :cache
             (fn [cache]
               (let [new-cache (assoc cache key {:content content
                                                  :timestamp (System/currentTimeMillis)})]
                 (if (> (count new-cache) max-size)
                   (into {} (take max-size (sort-by (comp :timestamp val) > new-cache)))
                   new-cache)))))))

(defn render-cached
  "Render with caching."
  [data format]
  (let [key (cache-key data format)]
    (or (get-cached key)
        (let [content (render data format)]
          (swap! state update-in [:stats :cache-misses] inc)
          (put-cached! key content)
          content))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-render
  "Ring middleware to render responses."
  [handler & {:keys [default-format] :or {default-format :json}}]
  (fn [request]
    (let [response (handler request)
          accept (get-in request [:headers "accept"])
          format (or (negotiate-format accept) default-format)]
      (if (map? (:body response))
        (let [rendered (render-response (:body response) format)]
          (merge response rendered))
        response))))

(defn wrap-content-negotiation
  "Ring middleware for content negotiation."
  [handler]
  (fn [request]
    (let [accept (get-in request [:headers "accept"])
          format (negotiate-format accept)]
      (handler (assoc request :negotiated-format format)))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-format!
  "Set default render format."
  [format]
  (swap! state assoc-in [:config :default-format] format))

(defn set-cache-enabled!
  "Enable/disable caching."
  [enabled?]
  (swap! state assoc-in [:config :cache-enabled?] enabled?))

(defn set-cache-ttl!
  "Set cache TTL."
  [ttl-ms]
  (swap! state assoc-in [:config :cache-ttl-ms] ttl-ms))

(defn clear-cache!
  "Clear the render cache."
  []
  (swap! state assoc :cache {}))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-renderer-metrics
  "Get renderer metrics."
  []
  (let [stats (:stats @state)]
    {:renders (:renders stats)
     :cache-hits (:cache-hits stats)
     :cache-misses (:cache-misses stats)
     :templates-rendered (:templates-rendered stats)
     :renderers-count (+ (count built-in-renderers)
                         (count (:renderers @state)))
     :templates-count (count (:templates @state))
     :cache-size (count (:cache @state))
     :cache-hit-rate (let [total (+ (:cache-hits stats) (:cache-misses stats))]
                       (if (pos? total)
                         (/ (:cache-hits stats) total)
                         0.0))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-renderer-stats
  "Get renderer statistics."
  []
  (merge (get-renderer-metrics)
         {:default-format (get-in @state [:config :default-format])
          :cache-enabled? (get-in @state [:config :cache-enabled?])
          :cache-ttl-ms (get-in @state [:config :cache-ttl-ms])}))

(defn reset-stats!
  "Reset renderer statistics."
  []
  (swap! state assoc :stats {:renders 0
                             :cache-hits 0
                             :cache-misses 0
                             :templates-rendered 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-renderer!
  "Initialize the response renderer."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response renderer initialized")
    (events/emit! :response-renderer-initialized {})
    true))
