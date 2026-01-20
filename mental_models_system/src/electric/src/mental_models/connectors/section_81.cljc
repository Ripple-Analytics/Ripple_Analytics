(ns mental-models.connectors.section-81
  "Connectors Module - Section 81"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clj-http.client :as http])
            #?(:clj [cheshire.core :as json])))

;; ============================================

(def streaming-config
  "Configuration for response streaming."
  (atom {:enabled true
         :buffer-size 8192
         :timeout-ms 30000}))

(defn get-streaming-config
  "Get current streaming configuration."
  []
  @streaming-config)

(defn set-streaming-config
  "Update streaming configuration."
  [config]
  (swap! streaming-config merge config))

(def active-streams
  "Registry of active streaming connections."
  (atom {}))

(defn register-stream
  "Register an active stream."
  [stream-id stream-info]
  (swap! active-streams assoc stream-id
         (merge stream-info
                {:started-at (System/currentTimeMillis)
                 :status :active})))

(defn unregister-stream
  "Unregister a stream."
  [stream-id]
  (swap! active-streams dissoc stream-id))

(defn get-active-streams
  "Get all active streams."
  []
  @active-streams)

#?(:clj
   (defn create-stream-handler
     "Create a handler for streaming responses."
     [stream-id callback]
     (let [buffer (java.io.ByteArrayOutputStream.)]
       (register-stream stream-id {:buffer buffer :callback callback})
       (fn [chunk]
         (when chunk
           (.write buffer (.getBytes chunk "UTF-8"))
           (when callback
             (callback chunk)))))))

(defn close-stream
  "Close a streaming connection."
  [stream-id]
  (when-let [stream (get @active-streams stream-id)]
    (swap! active-streams update stream-id assoc :status :closed :ended-at (System/currentTimeMillis))
    (unregister-stream stream-id)
    {:stream-id stream-id :status :closed}))

(defn get-stream-stats
  "Get statistics about streaming."
  []
  (let [streams (vals @active-streams)]
    {:active-count (count (filter #(= (:status %) :active) streams))
     :total-streams (count streams)}))

;; ============================================
;; Connector Versioning
;; ============================================

(def connector-versions
  "Version registry for connectors."
  (atom {}))

(def versioning-config
  "Configuration for connector versioning."
  (atom {:enabled true
         :default-version "1.0.0"
         :version-header "X-Connector-Version"
         :deprecation-warning-days 30}))

(defn get-versioning-config
  "Get current versioning configuration."
  []
  @versioning-config)

(defn set-versioning-config
  "Update versioning configuration."
  [config]
  (swap! versioning-config merge config))

(defn register-connector-version
  "Register a version for a connector."
  [connector-type version & {:keys [deprecated deprecated-date replacement-version changelog]}]
  (swap! connector-versions assoc-in [connector-type version]
         {:version version
          :registered-at (System/currentTimeMillis)
          :deprecated (boolean deprecated)
          :deprecated-date deprecated-date
          :replacement-version replacement-version
          :changelog changelog}))

(defn get-connector-version
  "Get version info for a connector."
  [connector-type version]
  (get-in @connector-versions [connector-type version]))

(defn get-latest-version
  "Get the latest non-deprecated version for a connector."
  [connector-type]
  (let [versions (get @connector-versions connector-type {})]
    (->> versions
         (filter (fn [[_ v]] (not (:deprecated v))))
         (sort-by (fn [[k _]] k))
         last
         first)))

(defn is-version-deprecated?
  "Check if a connector version is deprecated."
  [connector-type version]
  (get-in @connector-versions [connector-type version :deprecated] false))

(defn get-deprecation-warning
  "Get deprecation warning if applicable."
  [connector-type version]
  (when (is-version-deprecated? connector-type version)
    (let [info (get-connector-version connector-type version)]
      {:warning (str "Connector " (name connector-type) " version " version " is deprecated")
       :deprecated-date (:deprecated-date info)
       :replacement-version (:replacement-version info)})))

(defn list-connector-versions
  "List all versions for a connector."
  [connector-type]
  (let [versions (get @connector-versions connector-type {})]
    (mapv (fn [[k v]] (assoc v :version k)) versions)))

(defn get-version-stats
  "Get statistics about connector versions."
  []
  {:connectors (count @connector-versions)
   :total-versions (reduce + (map count (vals @connector-versions)))
   :deprecated-versions (count (filter :deprecated (mapcat vals (vals @connector-versions))))})

;; ============================================
;; Request Signing
