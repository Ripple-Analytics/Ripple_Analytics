(ns mental-models.pipeline.integration.response-paginator
  "Response paginator for mental model analysis system.
   
   Features:
   - Offset-based pagination
   - Cursor-based pagination
   - Keyset pagination
   - Page size limits
   - Pagination links
   - Total count handling
   - Pagination metadata
   - Pagination metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID Base64]
           [java.time Instant LocalDate]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:config {:default-page-size 20
                  :max-page-size 100
                  :min-page-size 1
                  :default-strategy :offset
                  :include-total? true}
         :stats {:requests-paginated 0
                 :offset-paginations 0
                 :cursor-paginations 0
                 :keyset-paginations 0}
         :initialized? false}))

;; ============================================================================
;; Pagination Parameters
;; ============================================================================

(defn parse-pagination-params
  "Parse pagination parameters from a request."
  [request]
  (let [params (merge (:params request) (:query-params request))
        page (some-> (get params "page") Integer/parseInt)
        page-size (some-> (get params "page_size") Integer/parseInt)
        offset (some-> (get params "offset") Integer/parseInt)
        limit (some-> (get params "limit") Integer/parseInt)
        cursor (get params "cursor")
        after (get params "after")
        before (get params "before")]
    {:page (or page 1)
     :page-size (or page-size limit (get-in @state [:config :default-page-size]))
     :offset (or offset (when page (* (dec page) (or page-size (get-in @state [:config :default-page-size])))))
     :cursor cursor
     :after after
     :before before
     :strategy (cond
                 cursor :cursor
                 (or after before) :keyset
                 :else :offset)}))

(defn validate-pagination-params
  "Validate pagination parameters."
  [params]
  (let [min-size (get-in @state [:config :min-page-size])
        max-size (get-in @state [:config :max-page-size])
        page-size (:page-size params)]
    (cond
      (< page-size min-size)
      {:valid? false :error (str "Page size must be at least " min-size)}
      
      (> page-size max-size)
      {:valid? false :error (str "Page size cannot exceed " max-size)}
      
      (and (:page params) (< (:page params) 1))
      {:valid? false :error "Page must be at least 1"}
      
      (and (:offset params) (< (:offset params) 0))
      {:valid? false :error "Offset cannot be negative"}
      
      :else
      {:valid? true :params params})))

;; ============================================================================
;; Cursor Encoding/Decoding
;; ============================================================================

(defn encode-cursor
  "Encode pagination state as a cursor."
  [data]
  (let [json-str (pr-str data)
        bytes (.getBytes json-str "UTF-8")]
    (.encodeToString (Base64/getUrlEncoder) bytes)))

(defn decode-cursor
  "Decode a cursor to pagination state."
  [cursor]
  (try
    (let [bytes (.decode (Base64/getUrlDecoder) cursor)
          json-str (String. bytes "UTF-8")]
      (edn/read-string json-str))
    (catch Exception _
      nil)))

;; ============================================================================
;; Offset-Based Pagination
;; ============================================================================

(defn paginate-offset
  "Apply offset-based pagination to a collection."
  [coll params]
  (swap! state update-in [:stats :offset-paginations] inc)
  
  (let [offset (or (:offset params) 0)
        limit (:page-size params)
        total (count coll)
        items (->> coll
                   (drop offset)
                   (take limit)
                   vec)
        has-more? (> total (+ offset limit))
        page (or (:page params) (inc (quot offset limit)))]
    {:items items
     :pagination {:strategy :offset
                  :page page
                  :page-size limit
                  :offset offset
                  :total total
                  :total-pages (int (Math/ceil (/ total limit)))
                  :has-next? has-more?
                  :has-prev? (pos? offset)}}))

;; ============================================================================
;; Cursor-Based Pagination
;; ============================================================================

(defn paginate-cursor
  "Apply cursor-based pagination to a collection."
  [coll params & {:keys [id-fn] :or {id-fn :id}}]
  (swap! state update-in [:stats :cursor-paginations] inc)
  
  (let [cursor (:cursor params)
        limit (:page-size params)
        cursor-data (when cursor (decode-cursor cursor))
        start-after (:after cursor-data)
        items (if start-after
                (->> coll
                     (drop-while #(not= (id-fn %) start-after))
                     rest
                     (take limit)
                     vec)
                (->> coll
                     (take limit)
                     vec))
        has-more? (> (count coll) (+ (count items) (if start-after 1 0)))
        next-cursor (when (and has-more? (seq items))
                      (encode-cursor {:after (id-fn (last items))}))]
    {:items items
     :pagination {:strategy :cursor
                  :cursor cursor
                  :next-cursor next-cursor
                  :has-next? has-more?
                  :page-size limit}}))

;; ============================================================================
;; Keyset-Based Pagination
;; ============================================================================

(defn paginate-keyset
  "Apply keyset-based pagination to a collection."
  [coll params & {:keys [key-fn comparator-fn]
                  :or {key-fn :id comparator-fn compare}}]
  (swap! state update-in [:stats :keyset-paginations] inc)
  
  (let [after (:after params)
        before (:before params)
        limit (:page-size params)
        filtered (cond
                   after (->> coll
                              (filter #(pos? (comparator-fn (key-fn %) after)))
                              (take limit)
                              vec)
                   before (->> coll
                               (filter #(neg? (comparator-fn (key-fn %) before)))
                               (take-last limit)
                               vec)
                   :else (->> coll
                              (take limit)
                              vec))
        has-next? (when (seq filtered)
                    (some #(pos? (comparator-fn (key-fn %) (key-fn (last filtered)))) coll))
        has-prev? (when (seq filtered)
                    (some #(neg? (comparator-fn (key-fn %) (key-fn (first filtered)))) coll))]
    {:items filtered
     :pagination {:strategy :keyset
                  :after after
                  :before before
                  :next-after (when has-next? (key-fn (last filtered)))
                  :prev-before (when has-prev? (key-fn (first filtered)))
                  :has-next? (boolean has-next?)
                  :has-prev? (boolean has-prev?)
                  :page-size limit}}))

;; ============================================================================
;; Unified Pagination
;; ============================================================================

(defn paginate
  "Paginate a collection using the appropriate strategy."
  [coll params & opts]
  (swap! state update-in [:stats :requests-paginated] inc)
  
  (let [strategy (or (:strategy params) (get-in @state [:config :default-strategy]))]
    (case strategy
      :cursor (apply paginate-cursor coll params opts)
      :keyset (apply paginate-keyset coll params opts)
      (paginate-offset coll params))))

;; ============================================================================
;; Pagination Links
;; ============================================================================

(defn generate-pagination-links
  "Generate pagination links for a response."
  [base-url pagination]
  (let [strategy (:strategy pagination)]
    (case strategy
      :offset
      (let [page (:page pagination)
            page-size (:page-size pagination)
            total-pages (:total-pages pagination)]
        (cond-> {:self (str base-url "?page=" page "&page_size=" page-size)}
          (:has-prev? pagination)
          (assoc :prev (str base-url "?page=" (dec page) "&page_size=" page-size))
          
          (:has-next? pagination)
          (assoc :next (str base-url "?page=" (inc page) "&page_size=" page-size))
          
          true
          (assoc :first (str base-url "?page=1&page_size=" page-size))
          
          (pos? total-pages)
          (assoc :last (str base-url "?page=" total-pages "&page_size=" page-size))))
      
      :cursor
      (cond-> {:self base-url}
        (:next-cursor pagination)
        (assoc :next (str base-url "?cursor=" (:next-cursor pagination))))
      
      :keyset
      (cond-> {:self base-url}
        (:next-after pagination)
        (assoc :next (str base-url "?after=" (:next-after pagination)))
        
        (:prev-before pagination)
        (assoc :prev (str base-url "?before=" (:prev-before pagination)))))))

;; ============================================================================
;; Response Formatting
;; ============================================================================

(defn format-paginated-response
  "Format a paginated response."
  [result base-url]
  (let [pagination (:pagination result)
        links (generate-pagination-links base-url pagination)]
    {:data (:items result)
     :pagination (assoc pagination :links links)
     :meta {:total (get pagination :total)
            :page (get pagination :page)
            :page-size (get pagination :page-size)}}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-pagination
  "Ring middleware to handle pagination."
  [handler]
  (fn [request]
    (let [params (parse-pagination-params request)
          validation (validate-pagination-params params)]
      (if (:valid? validation)
        (handler (assoc request :pagination-params params))
        {:status 400
         :body {:error (:error validation)}}))))

(defn wrap-paginate-response
  "Ring middleware to paginate response data."
  [handler & {:keys [data-key] :or {data-key :data}}]
  (fn [request]
    (let [response (handler request)
          data (get-in response [:body data-key])
          params (:pagination-params request)]
      (if (and data params (sequential? data))
        (let [result (paginate data params)
              base-url (:uri request)]
          (assoc response :body (format-paginated-response result base-url)))
        response))))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-default-page-size!
  "Set the default page size."
  [size]
  (swap! state assoc-in [:config :default-page-size] size))

(defn set-max-page-size!
  "Set the maximum page size."
  [size]
  (swap! state assoc-in [:config :max-page-size] size))

(defn set-default-strategy!
  "Set the default pagination strategy."
  [strategy]
  (swap! state assoc-in [:config :default-strategy] strategy))

(defn set-include-total!
  "Set whether to include total count."
  [include?]
  (swap! state assoc-in [:config :include-total?] include?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-paginator-metrics
  "Get paginator metrics."
  []
  (let [stats (:stats @state)]
    {:requests-paginated (:requests-paginated stats)
     :offset-paginations (:offset-paginations stats)
     :cursor-paginations (:cursor-paginations stats)
     :keyset-paginations (:keyset-paginations stats)}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-paginator-stats
  "Get paginator statistics."
  []
  (merge (get-paginator-metrics)
         {:default-page-size (get-in @state [:config :default-page-size])
          :max-page-size (get-in @state [:config :max-page-size])
          :default-strategy (get-in @state [:config :default-strategy])}))

(defn reset-stats!
  "Reset paginator statistics."
  []
  (swap! state assoc :stats {:requests-paginated 0
                             :offset-paginations 0
                             :cursor-paginations 0
                             :keyset-paginations 0}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-paginator!
  "Initialize the response paginator."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response paginator initialized")
    (events/emit! :response-paginator-initialized {})
    true))
