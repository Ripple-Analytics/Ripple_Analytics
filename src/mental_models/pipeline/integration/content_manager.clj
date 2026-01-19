(ns mental-models.pipeline.integration.content-manager
  "Content management for mental model analysis resources.
   
   Features:
   - Content CRUD operations
   - Content versioning
   - Content types
   - Media management
   - Content scheduling
   - Content workflow
   - Localization
   - SEO metadata"
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
  (atom {:content {}          ;; content-id -> content
         :versions {}         ;; content-id -> [versions]
         :media {}            ;; media-id -> media
         :content-types {}    ;; type-id -> content-type
         :workflows {}        ;; workflow-id -> workflow
         :locales {}          ;; locale -> {content-id -> localized}
         :stats {:content-created 0 :content-published 0 :media-uploaded 0}
         :initialized? false}))

;; ============================================================================
;; Content Type Management
;; ============================================================================

(defn define-content-type!
  "Define a content type."
  [type-id config]
  (let [content-type {:id type-id
                      :name (get config :name (name type-id))
                      :description (get config :description "")
                      :fields (get config :fields [])
                      :workflow-id (get config :workflow-id)
                      :icon (get config :icon)
                      :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:content-types type-id] content-type)
    (logging/log :info "Defined content type" {:type-id type-id})
    type-id))

(defn get-content-type
  "Get a content type."
  [type-id]
  (get-in @state [:content-types type-id]))

(defn list-content-types
  "List all content types."
  []
  (mapv (fn [[id ct]]
          {:id id
           :name (:name ct)
           :field-count (count (:fields ct))})
        (:content-types @state)))

;; ============================================================================
;; Content Operations
;; ============================================================================

(defn- generate-slug
  "Generate a URL-friendly slug."
  [title]
  (-> title
      str/lower-case
      (str/replace #"[^a-z0-9\s-]" "")
      (str/replace #"\s+" "-")
      (str/replace #"-+" "-")
      (str/trim)))

(defn create-content!
  "Create content."
  [content-id config]
  (when (flags/enabled? :content-manager)
    (let [content {:id content-id
                   :type (get config :type :article)
                   :title (get config :title "Untitled")
                   :slug (or (get config :slug) (generate-slug (get config :title "untitled")))
                   :body (get config :body "")
                   :excerpt (get config :excerpt "")
                   :author (get config :author)
                   :status (get config :status :draft) ;; :draft, :review, :published, :archived
                   :metadata (get config :metadata {})
                   :seo (get config :seo {})
                   :tags (get config :tags #{})
                   :categories (get config :categories #{})
                   :featured-media (get config :featured-media)
                   :publish-at (get config :publish-at)
                   :expire-at (get config :expire-at)
                   :version 1
                   :created-at (System/currentTimeMillis)
                   :updated-at (System/currentTimeMillis)}]
      
      (swap! state assoc-in [:content content-id] content)
      
      ;; Store initial version
      (swap! state assoc-in [:versions content-id]
             [{:version 1
               :title (:title content)
               :body (:body content)
               :created-at (System/currentTimeMillis)
               :created-by (:author content)}])
      
      (swap! state update-in [:stats :content-created] inc)
      (logging/log :info "Created content" {:content-id content-id})
      (events/emit! :content-created {:content-id content-id})
      content-id)))

(defn get-content
  "Get content."
  [content-id]
  (get-in @state [:content content-id]))

(defn list-content
  "List content."
  [& {:keys [type status author tag limit] :or {limit 100}}]
  (let [content (vals (:content @state))
        filtered (cond->> content
                   type (filter #(= (:type %) type))
                   status (filter #(= (:status %) status))
                   author (filter #(= (:author %) author))
                   tag (filter #(contains? (:tags %) tag))
                   true (sort-by :updated-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :title :slug :type :status :updated-at]) filtered)))

(defn update-content!
  "Update content."
  [content-id updates & {:keys [create-version?] :or {create-version? true}}]
  (when-let [content (get-content content-id)]
    (let [new-version (if create-version? (inc (:version content)) (:version content))
          updated (merge content
                         updates
                         {:version new-version
                          :updated-at (System/currentTimeMillis)})]
      
      (swap! state assoc-in [:content content-id] updated)
      
      ;; Store new version
      (when create-version?
        (swap! state update-in [:versions content-id]
               conj {:version new-version
                     :title (:title updated)
                     :body (:body updated)
                     :created-at (System/currentTimeMillis)}))
      
      (logging/log :info "Updated content" {:content-id content-id :version new-version})
      (events/emit! :content-updated {:content-id content-id})
      updated)))

(defn delete-content!
  "Delete content."
  [content-id]
  (swap! state update :content dissoc content-id)
  (swap! state update :versions dissoc content-id)
  (logging/log :info "Deleted content" {:content-id content-id})
  (events/emit! :content-deleted {:content-id content-id}))

;; ============================================================================
;; Content Workflow
;; ============================================================================

(defn publish-content!
  "Publish content."
  [content-id]
  (when-let [content (get-content content-id)]
    (update-content! content-id {:status :published
                                 :published-at (System/currentTimeMillis)}
                     :create-version? false)
    (swap! state update-in [:stats :content-published] inc)
    (logging/log :info "Published content" {:content-id content-id})
    (events/emit! :content-published {:content-id content-id})))

(defn unpublish-content!
  "Unpublish content."
  [content-id]
  (update-content! content-id {:status :draft} :create-version? false)
  (logging/log :info "Unpublished content" {:content-id content-id}))

(defn archive-content!
  "Archive content."
  [content-id]
  (update-content! content-id {:status :archived
                               :archived-at (System/currentTimeMillis)}
                   :create-version? false)
  (logging/log :info "Archived content" {:content-id content-id}))

(defn submit-for-review!
  "Submit content for review."
  [content-id]
  (update-content! content-id {:status :review
                               :submitted-at (System/currentTimeMillis)}
                   :create-version? false)
  (logging/log :info "Submitted for review" {:content-id content-id})
  (events/emit! :content-submitted-for-review {:content-id content-id}))

;; ============================================================================
;; Version Management
;; ============================================================================

(defn get-versions
  "Get all versions of content."
  [content-id]
  (get-in @state [:versions content-id] []))

(defn get-version
  "Get a specific version."
  [content-id version]
  (first (filter #(= (:version %) version) (get-versions content-id))))

(defn restore-version!
  "Restore content to a specific version."
  [content-id version]
  (when-let [version-data (get-version content-id version)]
    (update-content! content-id {:title (:title version-data)
                                 :body (:body version-data)})))

;; ============================================================================
;; Media Management
;; ============================================================================

(defn upload-media!
  "Upload media."
  [media-id config]
  (let [media {:id media-id
               :filename (get config :filename)
               :content-type (get config :content-type)
               :size (get config :size 0)
               :url (get config :url)
               :alt-text (get config :alt-text "")
               :caption (get config :caption "")
               :metadata (get config :metadata {})
               :uploaded-by (get config :uploaded-by)
               :uploaded-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:media media-id] media)
    (swap! state update-in [:stats :media-uploaded] inc)
    (logging/log :info "Uploaded media" {:media-id media-id})
    media-id))

(defn get-media
  "Get media."
  [media-id]
  (get-in @state [:media media-id]))

(defn list-media
  "List media."
  [& {:keys [content-type limit] :or {limit 100}}]
  (let [media (vals (:media @state))
        filtered (cond->> media
                   content-type (filter #(str/starts-with? (:content-type %) content-type))
                   true (sort-by :uploaded-at >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :filename :content-type :size :url]) filtered)))

(defn delete-media!
  "Delete media."
  [media-id]
  (swap! state update :media dissoc media-id)
  (logging/log :info "Deleted media" {:media-id media-id}))

;; ============================================================================
;; Localization
;; ============================================================================

(defn set-localized-content!
  "Set localized content."
  [content-id locale localized-data]
  (swap! state assoc-in [:locales locale content-id] localized-data)
  (logging/log :info "Set localized content" {:content-id content-id :locale locale}))

(defn get-localized-content
  "Get localized content."
  [content-id locale]
  (let [base-content (get-content content-id)
        localized (get-in @state [:locales locale content-id])]
    (if localized
      (merge base-content localized)
      base-content)))

(defn list-locales
  "List available locales for content."
  [content-id]
  (let [locales (keys (:locales @state))]
    (filter (fn [locale]
              (get-in @state [:locales locale content-id]))
            locales)))

;; ============================================================================
;; Content Scheduling
;; ============================================================================

(defn schedule-publish!
  "Schedule content for publishing."
  [content-id publish-at]
  (update-content! content-id {:publish-at publish-at
                               :status :scheduled}
                   :create-version? false)
  (logging/log :info "Scheduled publish" {:content-id content-id :publish-at publish-at}))

(defn process-scheduled-content!
  "Process scheduled content."
  []
  (let [now (System/currentTimeMillis)
        scheduled (filter (fn [[_ c]]
                            (and (= :scheduled (:status c))
                                 (:publish-at c)
                                 (<= (:publish-at c) now)))
                          (:content @state))]
    (doseq [[content-id _] scheduled]
      (publish-content! content-id))
    (count scheduled)))

(defn process-expired-content!
  "Process expired content."
  []
  (let [now (System/currentTimeMillis)
        expired (filter (fn [[_ c]]
                          (and (= :published (:status c))
                               (:expire-at c)
                               (<= (:expire-at c) now)))
                        (:content @state))]
    (doseq [[content-id _] expired]
      (archive-content! content-id))
    (count expired)))

;; ============================================================================
;; SEO
;; ============================================================================

(defn update-seo!
  "Update SEO metadata."
  [content-id seo-data]
  (update-content! content-id {:seo (merge (get-in @state [:content content-id :seo] {})
                                           seo-data)}
                   :create-version? false))

(defn get-seo
  "Get SEO metadata."
  [content-id]
  (get-in @state [:content content-id :seo] {}))

;; ============================================================================
;; Search
;; ============================================================================

(defn search-content
  "Search content."
  [query & {:keys [type status limit] :or {limit 50}}]
  (let [query-lower (str/lower-case query)
        content (vals (:content @state))
        scored (map (fn [c]
                      (let [title-match (if (str/includes? (str/lower-case (:title c "")) query-lower) 10 0)
                            body-match (if (str/includes? (str/lower-case (:body c "")) query-lower) 5 0)
                            tag-match (if (some #(str/includes? (str/lower-case (str %)) query-lower) (:tags c)) 3 0)
                            score (+ title-match body-match tag-match)]
                        (assoc c :search-score score)))
                    content)
        filtered (cond->> scored
                   (pos? (count query)) (filter #(pos? (:search-score %)))
                   type (filter #(= (:type %) type))
                   status (filter #(= (:status %) status))
                   true (sort-by :search-score >)
                   limit (take limit))]
    (mapv #(select-keys % [:id :title :slug :type :status :search-score]) filtered)))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-content-stats
  "Get content statistics."
  []
  (let [stats (:stats @state)
        content (vals (:content @state))
        by-status (frequencies (map :status content))
        by-type (frequencies (map :type content))]
    {:total-content (count content)
     :total-media (count (:media @state))
     :total-content-types (count (:content-types @state))
     :content-by-status by-status
     :content-by-type by-type
     :content-created (:content-created stats)
     :content-published (:content-published stats)
     :media-uploaded (:media-uploaded stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-content-manager!
  "Initialize the content manager."
  []
  (when-not (:initialized? @state)
    ;; Define content types
    (define-content-type! :article
                          {:name "Article"
                           :description "Standard article content"
                           :fields [{:name "title" :type :string :required true}
                                    {:name "body" :type :richtext :required true}
                                    {:name "excerpt" :type :text}
                                    {:name "featured-media" :type :media}]})
    
    (define-content-type! :mental-model
                          {:name "Mental Model"
                           :description "Mental model documentation"
                           :fields [{:name "title" :type :string :required true}
                                    {:name "description" :type :richtext :required true}
                                    {:name "category" :type :enum :options [:psychology :economics :physics :biology]}
                                    {:name "examples" :type :array}
                                    {:name "failure-modes" :type :array}]})
    
    (define-content-type! :case-study
                          {:name "Case Study"
                           :description "Mental model case study"
                           :fields [{:name "title" :type :string :required true}
                                    {:name "summary" :type :text}
                                    {:name "analysis" :type :richtext}
                                    {:name "models-applied" :type :array}
                                    {:name "outcome" :type :text}]})
    
    ;; Create sample content
    (create-content! :welcome-article
                     {:type :article
                      :title "Welcome to Mental Models System"
                      :body "This system helps you analyze documents using Charlie Munger's mental models framework."
                      :excerpt "Introduction to the Mental Models System"
                      :status :published
                      :tags #{:welcome :introduction}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Content manager initialized")
    (events/emit! :content-manager-initialized {})
    true))
