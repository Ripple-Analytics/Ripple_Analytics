(ns mental-models.pipeline.integration.response-wrapper
  "Response wrapper for mental model analysis system.
   
   Features:
   - Response envelope wrapping
   - Error wrapping
   - Metadata wrapping
   - Pagination wrapping
   - HATEOAS links
   - Custom wrappers
   - Wrapper profiles
   - Wrapping metrics"
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
  (atom {:wrappers {}         ;; wrapper-id -> wrapper
         :profiles {}         ;; profile-id -> profile
         :config {:default-envelope? true
                  :include-metadata? true
                  :include-links? false
                  :error-format :standard}
         :stats {:responses-wrapped 0
                 :errors-wrapped 0
                 :metadata-added 0
                 :links-added 0}
         :initialized? false}))

;; ============================================================================
;; Standard Envelope
;; ============================================================================

(defn wrap-in-envelope
  "Wrap response data in a standard envelope."
  [data & {:keys [success? message code]}]
  (swap! state update-in [:stats :responses-wrapped] inc)
  
  {:success (if (some? success?) success? true)
   :data data
   :message message
   :code code
   :timestamp (str (Instant/now))})

(defn wrap-error
  "Wrap an error in a standard format."
  [error & {:keys [code details stack-trace?]}]
  (swap! state update-in [:stats :errors-wrapped] inc)
  
  (let [error-format (get-in @state [:config :error-format])]
    (case error-format
      :standard
      {:success false
       :error {:message (if (instance? Throwable error)
                          (.getMessage error)
                          (str error))
               :code code
               :details details
               :timestamp (str (Instant/now))}
       :data nil}
      
      :simple
      {:error (str error)
       :code code}
      
      :detailed
      {:success false
       :error {:message (if (instance? Throwable error)
                          (.getMessage error)
                          (str error))
               :code code
               :type (when (instance? Throwable error)
                       (.getName (class error)))
               :details details
               :stack (when (and stack-trace? (instance? Throwable error))
                        (mapv str (.getStackTrace error)))
               :timestamp (str (Instant/now))}
       :data nil}
      
      {:error (str error)})))

;; ============================================================================
;; Metadata Wrapping
;; ============================================================================

(defn add-metadata
  "Add metadata to a response."
  [response & {:keys [request-id duration-ms version]}]
  (swap! state update-in [:stats :metadata-added] inc)
  
  (let [metadata {:timestamp (str (Instant/now))
                  :request-id request-id
                  :duration-ms duration-ms
                  :version version}]
    (if (map? response)
      (assoc response :_metadata (into {} (filter (comp some? val) metadata)))
      {:data response
       :_metadata (into {} (filter (comp some? val) metadata))})))

(defn add-request-context
  "Add request context to response."
  [response request]
  (let [context {:path (:uri request)
                 :method (name (or (:request-method request) :get))
                 :query-string (:query-string request)}]
    (if (map? response)
      (assoc response :_request (into {} (filter (comp some? val) context)))
      {:data response
       :_request (into {} (filter (comp some? val) context))})))

;; ============================================================================
;; Pagination Wrapping
;; ============================================================================

(defn wrap-paginated
  "Wrap paginated data."
  [data & {:keys [page page-size total-items total-pages has-next? has-prev?]}]
  (let [pagination {:page page
                    :page-size page-size
                    :total-items total-items
                    :total-pages (or total-pages
                                     (when (and total-items page-size)
                                       (int (Math/ceil (/ total-items page-size)))))
                    :has-next (or has-next?
                                  (when (and page total-pages)
                                    (< page total-pages)))
                    :has-prev (or has-prev?
                                  (when page (> page 1)))}]
    {:data data
     :pagination (into {} (filter (comp some? val) pagination))}))

(defn wrap-cursor-paginated
  "Wrap cursor-paginated data."
  [data & {:keys [cursor next-cursor prev-cursor has-more?]}]
  {:data data
   :pagination {:cursor cursor
                :next-cursor next-cursor
                :prev-cursor prev-cursor
                :has-more has-more?}})

;; ============================================================================
;; HATEOAS Links
;; ============================================================================

(defn add-links
  "Add HATEOAS links to response."
  [response links]
  (swap! state update-in [:stats :links-added] inc)
  
  (if (map? response)
    (assoc response :_links links)
    {:data response :_links links}))

(defn add-self-link
  "Add self link to response."
  [response uri]
  (add-links response {:self {:href uri}}))

(defn add-collection-links
  "Add collection links to response."
  [response & {:keys [self first-page last-page next-page prev-page]}]
  (let [links (cond-> {}
                self (assoc :self {:href self})
                first-page (assoc :first {:href first-page})
                last-page (assoc :last {:href last-page})
                next-page (assoc :next {:href next-page})
                prev-page (assoc :prev {:href prev-page}))]
    (add-links response links)))

(defn add-resource-links
  "Add resource links to response."
  [response resource-type resource-id base-url]
  (let [resource-url (str base-url "/" (name resource-type) "/" resource-id)]
    (add-links response
               {:self {:href resource-url}
                :collection {:href (str base-url "/" (name resource-type))}})))

;; ============================================================================
;; Custom Wrappers
;; ============================================================================

(defn register-wrapper!
  "Register a custom wrapper."
  [wrapper-id config]
  (let [wrapper {:id wrapper-id
                 :name (get config :name (name wrapper-id))
                 :wrap-fn (get config :wrap-fn)
                 :condition-fn (get config :condition-fn (constantly true))
                 :priority (get config :priority 100)
                 :enabled? (atom true)
                 :metrics {:invocations (atom 0)}
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:wrappers wrapper-id] wrapper)
    (logging/log :info "Registered wrapper" {:wrapper-id wrapper-id})
    wrapper-id))

(defn get-wrapper
  "Get a wrapper by ID."
  [wrapper-id]
  (get-in @state [:wrappers wrapper-id]))

(defn list-wrappers
  "List all wrappers."
  []
  (mapv (fn [[id w]]
          {:id id
           :name (:name w)
           :priority (:priority w)
           :enabled? @(:enabled? w)
           :invocations @(get-in w [:metrics :invocations])})
        (:wrappers @state)))

(defn apply-wrapper
  "Apply a specific wrapper."
  [wrapper-id response request]
  (when-let [wrapper (get-wrapper wrapper-id)]
    (when (and @(:enabled? wrapper)
               ((:condition-fn wrapper) request response))
      (swap! (get-in wrapper [:metrics :invocations]) inc)
      ((:wrap-fn wrapper) response request))))

(defn apply-all-wrappers
  "Apply all matching wrappers."
  [response request]
  (let [wrappers (->> (vals (:wrappers @state))
                      (filter #@(:enabled? %))
                      (filter #((:condition-fn %) request response))
                      (sort-by :priority))]
    (reduce (fn [resp wrapper]
              (swap! (get-in wrapper [:metrics :invocations]) inc)
              ((:wrap-fn wrapper) resp request))
            response
            wrappers)))

;; ============================================================================
;; Wrapper Profiles
;; ============================================================================

(defn register-profile!
  "Register a wrapper profile."
  [profile-id config]
  (let [profile {:id profile-id
                 :name (get config :name (name profile-id))
                 :envelope? (get config :envelope? true)
                 :metadata? (get config :metadata? true)
                 :links? (get config :links? false)
                 :pagination? (get config :pagination? false)
                 :wrappers (get config :wrappers [])
                 :enabled? (atom true)
                 :created-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:profiles profile-id] profile)
    profile-id))

(defn get-profile
  "Get a wrapper profile."
  [profile-id]
  (get-in @state [:profiles profile-id]))

(defn apply-profile
  "Apply a wrapper profile."
  [profile-id response request]
  (when-let [profile (get-profile profile-id)]
    (when @(:enabled? profile)
      (cond-> response
        (:envelope? profile) (wrap-in-envelope)
        (:metadata? profile) (add-metadata :request-id (:request-id request))
        (:links? profile) (add-self-link (:uri request))))))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-response-envelope
  "Ring middleware to wrap responses in envelope."
  [handler]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (and (map? body) (not (:success body)))
        (assoc response :body (wrap-in-envelope body))
        response))))

(defn wrap-response-metadata
  "Ring middleware to add metadata to responses."
  [handler]
  (fn [request]
    (let [start-time (System/currentTimeMillis)
          request-id (or (:request-id request) (str (UUID/randomUUID)))
          response (handler (assoc request :request-id request-id))
          duration-ms (- (System/currentTimeMillis) start-time)
          body (:body response)]
      (if (map? body)
        (assoc response :body (add-metadata body
                                            :request-id request-id
                                            :duration-ms duration-ms))
        response))))

(defn wrap-error-handler
  "Ring middleware to wrap errors."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Exception e
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (wrap-error e :code "INTERNAL_ERROR")}))))

(defn wrap-with-profile
  "Ring middleware to apply a wrapper profile."
  [handler profile-id]
  (fn [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (apply-profile profile-id body request))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-envelope!
  "Enable/disable default envelope wrapping."
  [enabled?]
  (swap! state assoc-in [:config :default-envelope?] enabled?))

(defn set-include-metadata!
  "Enable/disable metadata inclusion."
  [enabled?]
  (swap! state assoc-in [:config :include-metadata?] enabled?))

(defn set-include-links!
  "Enable/disable HATEOAS links."
  [enabled?]
  (swap! state assoc-in [:config :include-links?] enabled?))

(defn set-error-format!
  "Set the error format."
  [format]
  (swap! state assoc-in [:config :error-format] format))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-wrapper-metrics
  "Get wrapper metrics."
  []
  (let [stats (:stats @state)]
    {:responses-wrapped (:responses-wrapped stats)
     :errors-wrapped (:errors-wrapped stats)
     :metadata-added (:metadata-added stats)
     :links-added (:links-added stats)
     :wrappers-count (count (:wrappers @state))
     :profiles-count (count (:profiles @state))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-wrapper-stats
  "Get wrapper statistics."
  []
  (merge (get-wrapper-metrics)
         {:default-envelope? (get-in @state [:config :default-envelope?])
          :include-metadata? (get-in @state [:config :include-metadata?])
          :include-links? (get-in @state [:config :include-links?])
          :error-format (get-in @state [:config :error-format])}))

(defn reset-stats!
  "Reset wrapper statistics."
  []
  (swap! state assoc :stats {:responses-wrapped 0
                             :errors-wrapped 0
                             :metadata-added 0
                             :links-added 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-wrapper!
  "Initialize the response wrapper."
  []
  (when-not (:initialized? @state)
    ;; Register default profiles
    (register-profile! :api
                       {:name "API Profile"
                        :envelope? true
                        :metadata? true
                        :links? false})
    
    (register-profile! :hateoas
                       {:name "HATEOAS Profile"
                        :envelope? true
                        :metadata? true
                        :links? true})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Response wrapper initialized")
    (events/emit! :response-wrapper-initialized {})
    true))
