(ns mental-models.pipeline.integration.response-archiver
  "Response archiver for mental model analysis system.
   
   Features:
   - Response archiving
   - Archive storage
   - Archive retrieval
   - Archive compression
   - Archive retention
   - Archive search
   - Archive export
   - Archiver metrics"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout close!]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant LocalDate LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [java.io ByteArrayOutputStream]
           [java.util.zip GZIPOutputStream GZIPInputStream]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:archives {}         ;; archive-id -> archive data
         :index {}            ;; various indexes for search
         :config {:storage-path "/tmp/archives"
                  :compress? true
                  :retention-days 90
                  :max-archives 100000
                  :auto-archive? true}
         :stats {:archives-created (AtomicLong. 0)
                 :archives-retrieved (AtomicLong. 0)
                 :archives-deleted (AtomicLong. 0)
                 :bytes-archived (AtomicLong. 0)}
         :initialized? false}))

;; ============================================================================
;; Compression
;; ============================================================================

(defn compress-data
  "Compress data using GZIP."
  [data]
  (let [baos (ByteArrayOutputStream.)
        gzos (GZIPOutputStream. baos)
        bytes (if (string? data) (.getBytes data "UTF-8") data)]
    (.write gzos bytes)
    (.close gzos)
    (.toByteArray baos)))

(defn decompress-data
  "Decompress GZIP data."
  [data]
  (let [bais (java.io.ByteArrayInputStream. data)
        gzis (GZIPInputStream. bais)
        baos (ByteArrayOutputStream.)]
    (let [buffer (byte-array 1024)]
      (loop []
        (let [len (.read gzis buffer)]
          (when (pos? len)
            (.write baos buffer 0 len)
            (recur)))))
    (.close gzis)
    (.toByteArray baos)))

;; ============================================================================
;; Archive Creation
;; ============================================================================

(defn create-archive!
  "Create an archive entry."
  [archive-id data & {:keys [metadata tags compress?]
                      :or {compress? (get-in @state [:config :compress?])}}]
  (let [raw-bytes (if (string? data) (.getBytes data "UTF-8") (pr-str data))
        archived-data (if compress?
                        (compress-data raw-bytes)
                        raw-bytes)
        archive {:id archive-id
                 :data archived-data
                 :compressed? compress?
                 :original-size (count raw-bytes)
                 :archived-size (count archived-data)
                 :metadata (or metadata {})
                 :tags (set (or tags []))
                 :created-at (System/currentTimeMillis)
                 :accessed-at (atom (System/currentTimeMillis))}]
    
    (swap! state assoc-in [:archives archive-id] archive)
    (.incrementAndGet (:archives-created (:stats @state)))
    (.addAndGet (:bytes-archived (:stats @state)) (count archived-data))
    
    ;; Update indexes
    (doseq [tag (:tags archive)]
      (swap! state update-in [:index :by-tag tag] (fnil conj #{}) archive-id))
    
    (logging/log :debug "Created archive" {:archive-id archive-id :size (count archived-data)})
    archive-id))

(defn archive-response!
  "Archive a response."
  [response & {:keys [request-id tags metadata]}]
  (let [archive-id (or request-id (str (UUID/randomUUID)))
        data {:status (:status response)
              :headers (:headers response)
              :body (:body response)
              :archived-at (System/currentTimeMillis)}]
    (create-archive! archive-id data
                     :metadata (merge metadata {:type :response})
                     :tags tags)))

;; ============================================================================
;; Archive Retrieval
;; ============================================================================

(defn get-archive
  "Get an archive entry."
  [archive-id]
  (when-let [archive (get-in @state [:archives archive-id])]
    (.incrementAndGet (:archives-retrieved (:stats @state)))
    (reset! (:accessed-at archive) (System/currentTimeMillis))
    
    (let [data (if (:compressed? archive)
                 (decompress-data (:data archive))
                 (:data archive))]
      (assoc archive :data (String. data "UTF-8")))))

(defn get-archive-data
  "Get just the data from an archive."
  [archive-id]
  (when-let [archive (get-archive archive-id)]
    (try
      (edn/read-string (:data archive))
      (catch Exception _
        (:data archive)))))

(defn list-archives
  "List all archives."
  [& {:keys [limit offset tags start-time end-time]
      :or {limit 100 offset 0}}]
  (let [archives (vals (:archives @state))]
    (->> archives
         (filter (fn [a]
                   (and (or (nil? tags) (some (:tags a) tags))
                        (or (nil? start-time) (>= (:created-at a) start-time))
                        (or (nil? end-time) (<= (:created-at a) end-time)))))
         (sort-by :created-at >)
         (drop offset)
         (take limit)
         (mapv (fn [a]
                 {:id (:id a)
                  :original-size (:original-size a)
                  :archived-size (:archived-size a)
                  :compressed? (:compressed? a)
                  :tags (:tags a)
                  :created-at (:created-at a)})))))

;; ============================================================================
;; Archive Search
;; ============================================================================

(defn search-by-tag
  "Search archives by tag."
  [tag]
  (get-in @state [:index :by-tag tag] #{}))

(defn search-by-tags
  "Search archives by multiple tags (AND)."
  [tags]
  (reduce (fn [result tag]
            (clojure.set/intersection result (search-by-tag tag)))
          (search-by-tag (first tags))
          (rest tags)))

(defn search-archives
  "Search archives with criteria."
  [& {:keys [tags metadata-key metadata-value start-time end-time]}]
  (let [base-ids (if (seq tags)
                   (search-by-tags tags)
                   (set (keys (:archives @state))))]
    (->> base-ids
         (map #(get-in @state [:archives %]))
         (filter (fn [a]
                   (and (or (nil? metadata-key)
                            (= (get-in a [:metadata metadata-key]) metadata-value))
                        (or (nil? start-time) (>= (:created-at a) start-time))
                        (or (nil? end-time) (<= (:created-at a) end-time)))))
         (mapv :id))))

;; ============================================================================
;; Archive Deletion
;; ============================================================================

(defn delete-archive!
  "Delete an archive."
  [archive-id]
  (when-let [archive (get-in @state [:archives archive-id])]
    ;; Remove from indexes
    (doseq [tag (:tags archive)]
      (swap! state update-in [:index :by-tag tag] disj archive-id))
    
    (swap! state update :archives dissoc archive-id)
    (.incrementAndGet (:archives-deleted (:stats @state)))
    true))

(defn purge-old-archives!
  "Purge archives older than retention period."
  []
  (let [retention-days (get-in @state [:config :retention-days])
        cutoff (- (System/currentTimeMillis)
                  (* retention-days 24 60 60 1000))
        old-archives (filter (fn [[_ a]] (< (:created-at a) cutoff))
                             (:archives @state))]
    (doseq [[archive-id _] old-archives]
      (delete-archive! archive-id))
    (count old-archives)))

(defn clear-archives!
  "Clear all archives."
  []
  (swap! state assoc :archives {})
  (swap! state assoc :index {}))

;; ============================================================================
;; Archive Export
;; ============================================================================

(defn export-archive
  "Export an archive to a file."
  [archive-id file-path]
  (when-let [archive (get-archive archive-id)]
    (spit file-path (:data archive))
    file-path))

(defn export-archives
  "Export multiple archives."
  [archive-ids output-dir]
  (doseq [archive-id archive-ids]
    (let [file-path (str output-dir "/" archive-id ".edn")]
      (export-archive archive-id file-path)))
  output-dir)

(defn import-archive!
  "Import an archive from a file."
  [file-path & {:keys [archive-id tags]}]
  (let [data (slurp file-path)
        id (or archive-id (str (UUID/randomUUID)))]
    (create-archive! id data :tags tags)))

;; ============================================================================
;; Archive Statistics
;; ============================================================================

(defn get-archive-stats
  "Get statistics for an archive."
  [archive-id]
  (when-let [archive (get-in @state [:archives archive-id])]
    {:id archive-id
     :original-size (:original-size archive)
     :archived-size (:archived-size archive)
     :compression-ratio (if (pos? (:original-size archive))
                          (- 1.0 (/ (:archived-size archive) (:original-size archive)))
                          0.0)
     :compressed? (:compressed? archive)
     :tags (:tags archive)
     :created-at (:created-at archive)
     :last-accessed @(:accessed-at archive)
     :age-days (/ (- (System/currentTimeMillis) (:created-at archive))
                  (* 24 60 60 1000))}))

(defn get-storage-stats
  "Get storage statistics."
  []
  (let [archives (vals (:archives @state))]
    {:total-archives (count archives)
     :total-original-size (reduce + (map :original-size archives))
     :total-archived-size (reduce + (map :archived-size archives))
     :avg-compression-ratio (if (seq archives)
                              (let [total-original (reduce + (map :original-size archives))
                                    total-archived (reduce + (map :archived-size archives))]
                                (if (pos? total-original)
                                  (- 1.0 (/ total-archived total-original))
                                  0.0))
                              0.0)
     :oldest-archive (when (seq archives)
                       (:created-at (apply min-key :created-at archives)))
     :newest-archive (when (seq archives)
                       (:created-at (apply max-key :created-at archives)))}))

;; ============================================================================
;; Ring Middleware
;; ============================================================================

(defn wrap-archive
  "Ring middleware to archive responses."
  [handler & {:keys [tags]}]
  (fn [request]
    (let [response (handler request)
          request-id (or (get-in request [:headers "x-request-id"])
                         (str (UUID/randomUUID)))]
      (when (get-in @state [:config :auto-archive?])
        (archive-response! response
                           :request-id request-id
                           :tags tags
                           :metadata {:uri (:uri request)
                                      :method (:request-method request)}))
      (assoc-in response [:headers "X-Archive-Id"] request-id))))

(defn wrap-archive-conditional
  "Ring middleware to conditionally archive responses."
  [handler condition-fn & {:keys [tags]}]
  (fn [request]
    (let [response (handler request)]
      (when (condition-fn request response)
        (let [request-id (str (UUID/randomUUID))]
          (archive-response! response
                             :request-id request-id
                             :tags tags
                             :metadata {:uri (:uri request)
                                        :method (:request-method request)})))
      response)))

;; ============================================================================
;; Configuration
;; ============================================================================

(defn set-storage-path!
  "Set storage path."
  [path]
  (swap! state assoc-in [:config :storage-path] path))

(defn set-compress!
  "Enable/disable compression."
  [compress?]
  (swap! state assoc-in [:config :compress?] compress?))

(defn set-retention!
  "Set retention period in days."
  [days]
  (swap! state assoc-in [:config :retention-days] days))

(defn set-auto-archive!
  "Enable/disable auto-archiving."
  [auto?]
  (swap! state assoc-in [:config :auto-archive?] auto?))

;; ============================================================================
;; Metrics
;; ============================================================================

(defn get-archiver-metrics
  "Get archiver metrics."
  []
  (let [stats (:stats @state)]
    {:archives-created (.get (:archives-created stats))
     :archives-retrieved (.get (:archives-retrieved stats))
     :archives-deleted (.get (:archives-deleted stats))
     :bytes-archived (.get (:bytes-archived stats))
     :current-archives (count (:archives @state))
     :unique-tags (count (get-in @state [:index :by-tag]))}))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-archiver-stats
  "Get archiver statistics."
  []
  (merge (get-archiver-metrics)
         (get-storage-stats)
         {:storage-path (get-in @state [:config :storage-path])
          :compress? (get-in @state [:config :compress?])
          :retention-days (get-in @state [:config :retention-days])
          :auto-archive? (get-in @state [:config :auto-archive?])}))

(defn reset-stats!
  "Reset archiver statistics."
  []
  (.set (:archives-created (:stats @state)) 0)
  (.set (:archives-retrieved (:stats @state)) 0)
  (.set (:archives-deleted (:stats @state)) 0)
  (.set (:bytes-archived (:stats @state)) 0))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-response-archiver!
  "Initialize the response archiver."
  []
  (when-not (:initialized? @state)
    (swap! state assoc :initialized? true)
    (logging/log :info "Response archiver initialized")
    (events/emit! :response-archiver-initialized {})
    true))
