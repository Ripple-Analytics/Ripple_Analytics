(ns mental-models.pipeline.integration.document-store
  "Document store for mental model analysis documents.
   
   Features:
   - Document CRUD operations
   - Full-text indexing
   - Version control
   - Metadata management
   - Attachment handling
   - Document relationships
   - Access control
   - Bulk operations"
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan timeout]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [mental-models.infrastructure.flags :as flags]
            [mental-models.infrastructure.audit :as audit]
            [mental-models.infrastructure.metrics :as metrics]
            [mental-models.infrastructure.events :as events]
            [mental-models.infrastructure.logging :as logging])
  (:import [java.util UUID]
           [java.time Instant]
           [java.security MessageDigest]
           [java.util Base64]))

;; ============================================================================
;; State Management
;; ============================================================================

(defonce ^:private state
  (atom {:documents {}        ;; doc-id -> document
         :versions {}         ;; doc-id -> [versions]
         :attachments {}      ;; attachment-id -> attachment
         :relationships {}    ;; doc-id -> [relationships]
         :collections {}      ;; collection-id -> collection
         :index {}            ;; term -> [doc-ids]
         :stats {:documents-created 0 :documents-updated 0 :searches 0}
         :initialized? false}))

;; ============================================================================
;; Document Operations
;; ============================================================================

(defn- compute-hash
  "Compute content hash."
  [content]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes (.digest digest (.getBytes (str content) "UTF-8"))]
    (.encodeToString (Base64/getEncoder) bytes)))

(defn create-document!
  "Create a document."
  [doc-id config]
  (let [content (get config :content "")
        document {:id doc-id
                  :title (get config :title "Untitled")
                  :content content
                  :content-type (get config :content-type "text/plain")
                  :metadata (get config :metadata {})
                  :tags (get config :tags #{})
                  :collection-id (get config :collection-id)
                  :owner (get config :owner)
                  :permissions (get config :permissions {:read #{:all} :write #{:owner}})
                  :version 1
                  :content-hash (compute-hash content)
                  :status (get config :status :draft)
                  :created-at (System/currentTimeMillis)
                  :updated-at (System/currentTimeMillis)}]
    
    (swap! state assoc-in [:documents doc-id] document)
    
    ;; Store initial version
    (swap! state assoc-in [:versions doc-id]
           [{:version 1
             :content content
             :content-hash (:content-hash document)
             :created-at (System/currentTimeMillis)
             :created-by (:owner document)}])
    
    ;; Index document
    (index-document! doc-id)
    
    ;; Add to collection
    (when (:collection-id document)
      (swap! state update-in [:collections (:collection-id document) :documents]
             (fnil conj #{}) doc-id))
    
    (swap! state update-in [:stats :documents-created] inc)
    (logging/log :info "Created document" {:doc-id doc-id})
    (events/emit! :document-created {:doc-id doc-id})
    doc-id))

(defn get-document
  "Get a document."
  [doc-id]
  (get-in @state [:documents doc-id]))

(defn list-documents
  "List documents."
  [& {:keys [collection-id owner tag status limit] :or {limit 100}}]
  (let [documents (vals (:documents @state))
        filtered (cond->> documents
                   collection-id (filter #(= (:collection-id %) collection-id))
                   owner (filter #(= (:owner %) owner))
                   tag (filter #(contains? (:tags %) tag))
                   status (filter #(= (:status %) status))
                   true (take limit))]
    (mapv #(select-keys % [:id :title :status :version :updated-at]) filtered)))

(defn update-document!
  "Update a document."
  [doc-id updates & {:keys [create-version?] :or {create-version? true}}]
  (when-let [document (get-document doc-id)]
    (let [new-content (get updates :content (:content document))
          new-version (if create-version? (inc (:version document)) (:version document))
          updated-doc (merge document
                             updates
                             {:version new-version
                              :content-hash (compute-hash new-content)
                              :updated-at (System/currentTimeMillis)})]
      
      (swap! state assoc-in [:documents doc-id] updated-doc)
      
      ;; Store new version
      (when create-version?
        (swap! state update-in [:versions doc-id]
               conj {:version new-version
                     :content new-content
                     :content-hash (:content-hash updated-doc)
                     :created-at (System/currentTimeMillis)}))
      
      ;; Re-index document
      (index-document! doc-id)
      
      (swap! state update-in [:stats :documents-updated] inc)
      (logging/log :info "Updated document" {:doc-id doc-id :version new-version})
      (events/emit! :document-updated {:doc-id doc-id :version new-version})
      updated-doc)))

(defn delete-document!
  "Delete a document."
  [doc-id]
  (when-let [document (get-document doc-id)]
    ;; Remove from collection
    (when (:collection-id document)
      (swap! state update-in [:collections (:collection-id document) :documents]
             disj doc-id))
    
    ;; Remove from index
    (unindex-document! doc-id)
    
    ;; Remove document and versions
    (swap! state update :documents dissoc doc-id)
    (swap! state update :versions dissoc doc-id)
    (swap! state update :relationships dissoc doc-id)
    
    (logging/log :info "Deleted document" {:doc-id doc-id})
    (events/emit! :document-deleted {:doc-id doc-id})))

;; ============================================================================
;; Version Control
;; ============================================================================

(defn get-versions
  "Get all versions of a document."
  [doc-id]
  (get-in @state [:versions doc-id] []))

(defn get-version
  "Get a specific version of a document."
  [doc-id version]
  (first (filter #(= (:version %) version) (get-versions doc-id))))

(defn restore-version!
  "Restore a document to a specific version."
  [doc-id version]
  (when-let [version-data (get-version doc-id version)]
    (update-document! doc-id {:content (:content version-data)})))

(defn compare-versions
  "Compare two versions of a document."
  [doc-id version1 version2]
  (let [v1 (get-version doc-id version1)
        v2 (get-version doc-id version2)]
    (when (and v1 v2)
      {:version1 version1
       :version2 version2
       :content1 (:content v1)
       :content2 (:content v2)
       :same? (= (:content-hash v1) (:content-hash v2))})))

;; ============================================================================
;; Full-Text Indexing
;; ============================================================================

(defn- tokenize
  "Tokenize text for indexing."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" " ")
        (str/split #"\s+")
        (->> (filter #(> (count %) 2))
             distinct))))

(defn- index-document!
  "Index a document for full-text search."
  [doc-id]
  (when-let [document (get-document doc-id)]
    (let [tokens (concat (tokenize (:title document))
                         (tokenize (:content document))
                         (mapcat tokenize (:tags document)))]
      (doseq [token tokens]
        (swap! state update-in [:index token] (fnil conj #{}) doc-id)))))

(defn- unindex-document!
  "Remove a document from the index."
  [doc-id]
  (swap! state update :index
         (fn [idx]
           (into {}
                 (map (fn [[term docs]]
                        [term (disj docs doc-id)])
                      idx)))))

(defn search-documents
  "Search documents."
  [query & {:keys [collection-id owner limit] :or {limit 50}}]
  (swap! state update-in [:stats :searches] inc)
  (let [tokens (tokenize query)
        ;; Find matching doc IDs
        matching-ids (if (seq tokens)
                       (reduce (fn [acc token]
                                 (let [matches (get-in @state [:index token] #{})]
                                   (if (empty? acc)
                                     matches
                                     (clojure.set/intersection acc matches))))
                               #{}
                               tokens)
                       #{})
        ;; Get documents and score
        documents (map get-document matching-ids)
        scored (map (fn [doc]
                      (let [title-matches (count (filter #(str/includes? (str/lower-case (:title doc "")) %) tokens))
                            content-matches (count (filter #(str/includes? (str/lower-case (:content doc "")) %) tokens))
                            score (+ (* title-matches 10) content-matches)]
                        (assoc doc :search-score score)))
                    documents)
        ;; Filter and sort
        filtered (cond->> scored
                   collection-id (filter #(= (:collection-id %) collection-id))
                   owner (filter #(= (:owner %) owner))
                   true (sort-by :search-score >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :title :status :search-score]) filtered)))

;; ============================================================================
;; Attachments
;; ============================================================================

(defn add-attachment!
  "Add an attachment to a document."
  [doc-id attachment-config]
  (when (get-document doc-id)
    (let [attachment-id (str (UUID/randomUUID))
          attachment {:id attachment-id
                      :doc-id doc-id
                      :filename (get attachment-config :filename)
                      :content-type (get attachment-config :content-type)
                      :size (get attachment-config :size 0)
                      :data (get attachment-config :data)
                      :created-at (System/currentTimeMillis)}]
      (swap! state assoc-in [:attachments attachment-id] attachment)
      (swap! state update-in [:documents doc-id :attachments]
             (fnil conj []) attachment-id)
      (logging/log :info "Added attachment" {:doc-id doc-id :attachment-id attachment-id})
      attachment-id)))

(defn get-attachment
  "Get an attachment."
  [attachment-id]
  (get-in @state [:attachments attachment-id]))

(defn list-attachments
  "List attachments for a document."
  [doc-id]
  (let [attachment-ids (get-in @state [:documents doc-id :attachments] [])]
    (mapv (fn [id]
            (let [a (get-attachment id)]
              (select-keys a [:id :filename :content-type :size :created-at])))
          attachment-ids)))

(defn delete-attachment!
  "Delete an attachment."
  [attachment-id]
  (when-let [attachment (get-attachment attachment-id)]
    (swap! state update-in [:documents (:doc-id attachment) :attachments]
           (fn [atts] (vec (remove #(= % attachment-id) atts))))
    (swap! state update :attachments dissoc attachment-id)
    (logging/log :info "Deleted attachment" {:attachment-id attachment-id})))

;; ============================================================================
;; Relationships
;; ============================================================================

(defn add-relationship!
  "Add a relationship between documents."
  [from-doc-id to-doc-id relationship-type]
  (when (and (get-document from-doc-id) (get-document to-doc-id))
    (let [relationship {:from from-doc-id
                        :to to-doc-id
                        :type relationship-type
                        :created-at (System/currentTimeMillis)}]
      (swap! state update-in [:relationships from-doc-id]
             (fnil conj []) relationship)
      (logging/log :info "Added relationship" {:from from-doc-id :to to-doc-id :type relationship-type}))))

(defn get-relationships
  "Get relationships for a document."
  [doc-id & {:keys [type direction] :or {direction :outgoing}}]
  (case direction
    :outgoing (let [rels (get-in @state [:relationships doc-id] [])]
                (if type
                  (filter #(= (:type %) type) rels)
                  rels))
    :incoming (let [all-rels (mapcat val (:relationships @state))]
                (filter (fn [r]
                          (and (= (:to r) doc-id)
                               (or (nil? type) (= (:type r) type))))
                        all-rels))
    :both (concat (get-relationships doc-id :type type :direction :outgoing)
                  (get-relationships doc-id :type type :direction :incoming))))

(defn remove-relationship!
  "Remove a relationship."
  [from-doc-id to-doc-id relationship-type]
  (swap! state update-in [:relationships from-doc-id]
         (fn [rels]
           (vec (remove #(and (= (:to %) to-doc-id)
                              (= (:type %) relationship-type))
                        rels)))))

;; ============================================================================
;; Collections
;; ============================================================================

(defn create-collection!
  "Create a document collection."
  [collection-id config]
  (let [collection {:id collection-id
                    :name (get config :name (name collection-id))
                    :description (get config :description "")
                    :owner (get config :owner)
                    :documents #{}
                    :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:collections collection-id] collection)
    (logging/log :info "Created collection" {:collection-id collection-id})
    collection-id))

(defn get-collection
  "Get a collection."
  [collection-id]
  (get-in @state [:collections collection-id]))

(defn list-collections
  "List all collections."
  []
  (mapv (fn [[id c]]
          {:id id
           :name (:name c)
           :document-count (count (:documents c))})
        (:collections @state)))

(defn delete-collection!
  "Delete a collection."
  [collection-id & {:keys [delete-documents?]}]
  (when-let [collection (get-collection collection-id)]
    (when delete-documents?
      (doseq [doc-id (:documents collection)]
        (delete-document! doc-id)))
    (swap! state update :collections dissoc collection-id)
    (logging/log :info "Deleted collection" {:collection-id collection-id})))

;; ============================================================================
;; Bulk Operations
;; ============================================================================

(defn bulk-create!
  "Create multiple documents."
  [documents]
  (mapv (fn [doc]
          (create-document! (or (:id doc) (str (UUID/randomUUID))) doc))
        documents))

(defn bulk-update!
  "Update multiple documents."
  [updates]
  (mapv (fn [{:keys [id] :as update}]
          (update-document! id (dissoc update :id)))
        updates))

(defn bulk-delete!
  "Delete multiple documents."
  [doc-ids]
  (doseq [doc-id doc-ids]
    (delete-document! doc-id))
  (count doc-ids))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-store-stats
  "Get document store statistics."
  []
  (let [stats (:stats @state)
        documents (vals (:documents @state))
        by-status (frequencies (map :status documents))]
    {:total-documents (count documents)
     :total-versions (reduce + (map count (vals (:versions @state))))
     :total-attachments (count (:attachments @state))
     :total-collections (count (:collections @state))
     :index-terms (count (:index @state))
     :documents-by-status by-status
     :documents-created (:documents-created stats)
     :documents-updated (:documents-updated stats)
     :searches (:searches stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-document-store!
  "Initialize the document store."
  []
  (when-not (:initialized? @state)
    ;; Create default collection
    (create-collection! :default
                        {:name "Default Collection"
                         :description "Default document collection"})
    
    (create-collection! :analysis-results
                        {:name "Analysis Results"
                         :description "Mental model analysis results"})
    
    ;; Create sample document
    (create-document! :welcome
                      {:title "Welcome to Mental Models System"
                       :content "This system helps you analyze documents using Charlie Munger's mental models framework."
                       :collection-id :default
                       :tags #{:welcome :documentation}
                       :status :published})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Document store initialized")
    (events/emit! :document-store-initialized {})
    true))
