(ns mental-models.pipeline.integration.response-minifier
  "Response minifier for mental model analysis system.
   
   Features:
   - JSON minification
   - HTML minification
   - CSS minification
   - JavaScript minification
   - Whitespace removal
   - Comment removal
   - Field filtering
   - Minification metrics"
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
  (atom {:config {:minify-json? true
                  :minify-html? true
                  :minify-css? true
                  :minify-js? true
                  :remove-nulls? true
                  :remove-empty-strings? false
                  :remove-empty-arrays? false
                  :exclude-fields #{}
                  :include-fields nil}
         :stats {:minifications 0
                 :json-minifications 0
                 :html-minifications 0
                 :css-minifications 0
                 :js-minifications 0
                 :bytes-saved 0}
         :initialized? false}))

;; ============================================================================
;; JSON Minification
;; ============================================================================

(defn- remove-nulls
  "Remove null values from a map."
  [m]
  (into {} (remove (fn [[_ v]] (nil? v)) m)))

(defn- remove-empty-strings
  "Remove empty string values from a map."
  [m]
  (into {} (remove (fn [[_ v]] (and (string? v) (empty? v))) m)))

(defn- remove-empty-arrays
  "Remove empty array values from a map."
  [m]
  (into {} (remove (fn [[_ v]] (and (sequential? v) (empty? v))) m)))

(defn- filter-fields
  "Filter fields based on include/exclude lists."
  [m]
  (let [exclude-fields (get-in @state [:config :exclude-fields])
        include-fields (get-in @state [:config :include-fields])]
    (cond
      (seq include-fields)
      (select-keys m include-fields)
      
      (seq exclude-fields)
      (apply dissoc m exclude-fields)
      
      :else m)))

(defn- minify-map
  "Minify a map recursively."
  [m]
  (let [config (:config @state)]
    (cond-> m
      true filter-fields
      (:remove-nulls? config) remove-nulls
      (:remove-empty-strings? config) remove-empty-strings
      (:remove-empty-arrays? config) remove-empty-arrays
      true (update-vals (fn [v]
                          (cond
                            (map? v) (minify-map v)
                            (sequential? v) (mapv #(if (map? %) (minify-map %) %) v)
                            :else v))))))

(defn minify-json
  "Minify JSON data."
  [data]
  (swap! state update-in [:stats :json-minifications] inc)
  (if (map? data)
    (minify-map data)
    data))

(defn minify-json-string
  "Minify a JSON string by removing whitespace."
  [json-str]
  (swap! state update-in [:stats :json-minifications] inc)
  (let [original-size (count json-str)
        minified (-> json-str
                     (str/replace #"\s+" " ")
                     (str/replace #"\s*:\s*" ":")
                     (str/replace #"\s*,\s*" ",")
                     (str/replace #"\s*\{\s*" "{")
                     (str/replace #"\s*\}\s*" "}")
                     (str/replace #"\s*\[\s*" "[")
                     (str/replace #"\s*\]\s*" "]")
                     str/trim)
        saved (- original-size (count minified))]
    (swap! state update-in [:stats :bytes-saved] + saved)
    minified))

;; ============================================================================
;; HTML Minification
;; ============================================================================

(defn- remove-html-comments
  "Remove HTML comments."
  [html]
  (str/replace html #"<!--[\s\S]*?-->" ""))

(defn- collapse-html-whitespace
  "Collapse whitespace in HTML."
  [html]
  (-> html
      (str/replace #">\s+<" "><")
      (str/replace #"\s+" " ")
      str/trim))

(defn- remove-optional-tags
  "Remove optional closing tags."
  [html]
  (-> html
      (str/replace #"</li>" "")
      (str/replace #"</dt>" "")
      (str/replace #"</dd>" "")
      (str/replace #"</p>" "")
      (str/replace #"</option>" "")))

(defn minify-html
  "Minify HTML content."
  [html]
  (swap! state update-in [:stats :html-minifications] inc)
  (let [original-size (count html)
        minified (-> html
                     remove-html-comments
                     collapse-html-whitespace)
        saved (- original-size (count minified))]
    (swap! state update-in [:stats :bytes-saved] + saved)
    minified))

;; ============================================================================
;; CSS Minification
;; ============================================================================

(defn- remove-css-comments
  "Remove CSS comments."
  [css]
  (str/replace css #"/\*[\s\S]*?\*/" ""))

(defn- collapse-css-whitespace
  "Collapse whitespace in CSS."
  [css]
  (-> css
      (str/replace #"\s+" " ")
      (str/replace #"\s*\{\s*" "{")
      (str/replace #"\s*\}\s*" "}")
      (str/replace #"\s*:\s*" ":")
      (str/replace #"\s*;\s*" ";")
      (str/replace #"\s*,\s*" ",")
      (str/replace #";}" "}")
      str/trim))

(defn minify-css
  "Minify CSS content."
  [css]
  (swap! state update-in [:stats :css-minifications] inc)
  (let [original-size (count css)
        minified (-> css
                     remove-css-comments
                     collapse-css-whitespace)
        saved (- original-size (count minified))]
    (swap! state update-in [:stats :bytes-saved] + saved)
    minified))

;; ============================================================================
;; JavaScript Minification
;; ============================================================================

(defn- remove-js-comments
  "Remove JavaScript comments (simple implementation)."
  [js]
  (-> js
      (str/replace #"//[^\n]*" "")
      (str/replace #"/\*[\s\S]*?\*/" "")))

(defn- collapse-js-whitespace
  "Collapse whitespace in JavaScript."
  [js]
  (-> js
      (str/replace #"\s+" " ")
      (str/replace #"\s*\{\s*" "{")
      (str/replace #"\s*\}\s*" "}")
      (str/replace #"\s*\(\s*" "(")
      (str/replace #"\s*\)\s*" ")")
      (str/replace #"\s*;\s*" ";")
      (str/replace #"\s*,\s*" ",")
      (str/replace #"\s*=\s*" "=")
      str/trim))

(defn minify-js
  "Minify JavaScript content (basic minification)."
  [js]
  (swap! state update-in [:stats :js-minifications] inc)
  (let [original-size (count js)
        minified (-> js
                     remove-js-comments
                     collapse-js-whitespace)
        saved (- original-size (count minified))]
    (swap! state update-in [:stats :bytes-saved] + saved)
    minified))

;; ============================================================================
;; Content Type Detection
;; ============================================================================

(defn- detect-content-type
  "Detect content type from response."
  [response]
  (let [content-type (get-in response [:headers "Content-Type"] "")]
    (cond
      (str/includes? content-type "application/json") :json
      (str/includes? content-type "text/html") :html
      (str/includes? content-type "text/css") :css
      (str/includes? content-type "application/javascript") :js
      (str/includes? content-type "text/javascript") :js
      :else :unknown)))

;; ============================================================================
;; Response Minification
;; ============================================================================

(defn minify-response
  "Minify a response based on content type."
  [response]
  (swap! state update-in [:stats :minifications] inc)
  
  (let [content-type (detect-content-type response)
        body (:body response)
        config (:config @state)]
    (case content-type
      :json (if (and (:minify-json? config) (map? body))
              (assoc response :body (minify-json body))
              (if (and (:minify-json? config) (string? body))
                (assoc response :body (minify-json-string body))
                response))
      
      :html (if (and (:minify-html? config) (string? body))
              (assoc response :body (minify-html body))
              response)
      
      :css (if (and (:minify-css? config) (string? body))
             (assoc response :body (minify-css body))
             response)
      
      :js (if (and (:minify-js? config) (string? body))
            (assoc response :body (minify-js body))
            response)
      
      response)))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-minify
  "Ring middleware to minify responses."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (minify-response response))))

(defn wrap-minify-json
  "Ring middleware to minify JSON responses only."
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (= :json (detect-content-type response))
        (minify-response response)
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-minify-json!
  "Enable/disable JSON minification."
  [enabled?]
  (swap! state assoc-in [:config :minify-json?] enabled?))

(defn set-minify-html!
  "Enable/disable HTML minification."
  [enabled?]
  (swap! state assoc-in [:config :minify-html?] enabled?))

(defn set-minify-css!
  "Enable/disable CSS minification."
  [enabled?]
  (swap! state assoc-in [:config :minify-css?] enabled?))

(defn set-minify-js!
  "Enable/disable JavaScript minification."
  [enabled?]
  (swap! state assoc-in [:config :minify-js?] enabled?))

(defn set-remove-nulls!
  "Enable/disable null removal."
  [enabled?]
  (swap! state assoc-in [:config :remove-nulls?] enabled?))

(defn set-exclude-fields!
  "Set fields to exclude from responses."
  [fields]
  (swap! state assoc-in [:config :exclude-fields] (set fields)))

(defn set-include-fields!
  "Set fields to include in responses (whitelist)."
  [fields]
  (swap! state assoc-in [:config :include-fields] (set fields)))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-minifier-metrics
  "Get minifier metrics."
  []
  (let [stats (:stats @state)]
    {:minifications (:minifications stats)
     :json-minifications (:json-minifications stats)
     :html-minifications (:html-minifications stats)
     :css-minifications (:css-minifications stats)
     :js-minifications (:js-minifications stats)
     :bytes-saved (:bytes-saved stats)
     :bytes-saved-kb (/ (:bytes-saved stats) 1024.0)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-minifier-stats
  "Get minifier statistics."
  []
  (merge (get-minifier-metrics)
         {:minify-json? (get-in @state [:config :minify-json?])
          :minify-html? (get-in @state [:config :minify-html?])
          :minify-css? (get-in @state [:config :minify-css?])
          :minify-js? (get-in @state [:config :minify-js?])
          :remove-nulls? (get-in @state [:config :remove-nulls?])}))

(defn reset-stats!
  "Reset minifier statistics."
  []
  (swap! state assoc :stats {:minifications 0
                             :json-minifications 0
                             :html-minifications 0
                             :css-minifications 0
                             :js-minifications 0
                             :bytes-saved 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-minifier!
  "Initialize the response minifier."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response minifier initialized")
    (events/emit! :response-minifier-initialized {})
    true))
