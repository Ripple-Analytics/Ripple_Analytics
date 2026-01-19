(ns mental-models.pipeline.integration.documentation-generator
  "Documentation generator for mental model analysis system.
   
   Features:
   - API documentation
   - Code documentation
   - User guides
   - Changelog generation
   - Diagram generation
   - Search indexing
   - Version management
   - Export formats"
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
  (atom {:documents {}        ;; doc-id -> document
         :sections {}         ;; section-id -> section
         :api-specs {}        ;; spec-id -> api-spec
         :changelogs {}       ;; version -> changelog
         :diagrams {}         ;; diagram-id -> diagram
         :search-index {}     ;; term -> [doc-ids]
         :config {:output-format :markdown
                  :include-examples? true
                  :include-diagrams? true}
         :stats {:documents-generated 0 :api-specs-generated 0}
         :initialized? false}))

;; ============================================================================
;; Document Management
;; ============================================================================

(defn create-document!
  "Create a document."
  [doc-id config]
  (let [document {:id doc-id
                  :title (get config :title (name doc-id))
                  :description (get config :description "")
                  :type (get config :type :guide) ;; :guide, :reference, :tutorial, :api
                  :sections (get config :sections [])
                  :tags (get config :tags #{})
                  :version (get config :version "1.0.0")
                  :author (get config :author)
                  :status :draft ;; :draft, :review, :published
                  :created-at (System/currentTimeMillis)
                  :updated-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:documents doc-id] document)
    (logging/log :info "Created document" {:doc-id doc-id})
    doc-id))

(defn get-document
  "Get a document."
  [doc-id]
  (get-in @state [:documents doc-id]))

(defn list-documents
  "List all documents."
  [& {:keys [type status tag]}]
  (let [documents (vals (:documents @state))
        filtered (cond->> documents
                   type (filter #(= (:type %) type))
                   status (filter #(= (:status %) status))
                   tag (filter #(contains? (:tags %) tag)))]
    (mapv #(select-keys % [:id :title :type :status :version]) filtered)))

(defn update-document!
  "Update a document."
  [doc-id updates]
  (swap! state update-in [:documents doc-id]
         (fn [d]
           (merge d updates {:updated-at (System/currentTimeMillis)}))))

(defn publish-document!
  "Publish a document."
  [doc-id]
  (update-document! doc-id {:status :published :published-at (System/currentTimeMillis)})
  (index-document! doc-id)
  (logging/log :info "Published document" {:doc-id doc-id}))

;; ============================================================================
;; Section Management
;; ============================================================================

(defn create-section!
  "Create a section."
  [section-id config]
  (let [section {:id section-id
                 :title (get config :title (name section-id))
                 :content (get config :content "")
                 :order (get config :order 0)
                 :parent-id (get config :parent-id)
                 :subsections (get config :subsections [])
                 :examples (get config :examples [])
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:sections section-id] section)
    section-id))

(defn get-section
  "Get a section."
  [section-id]
  (get-in @state [:sections section-id]))

(defn add-section-to-document!
  "Add a section to a document."
  [doc-id section-id]
  (swap! state update-in [:documents doc-id :sections] conj section-id))

;; ============================================================================
;; API Documentation
;; ============================================================================

(defn create-api-spec!
  "Create an API specification."
  [spec-id config]
  (let [spec {:id spec-id
              :title (get config :title (name spec-id))
              :version (get config :version "1.0.0")
              :base-url (get config :base-url)
              :endpoints (get config :endpoints [])
              :schemas (get config :schemas {})
              :authentication (get config :authentication)
              :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:api-specs spec-id] spec)
    (swap! state update-in [:stats :api-specs-generated] inc)
    (logging/log :info "Created API spec" {:spec-id spec-id})
    spec-id))

(defn get-api-spec
  "Get an API specification."
  [spec-id]
  (get-in @state [:api-specs spec-id]))

(defn add-endpoint!
  "Add an endpoint to API spec."
  [spec-id endpoint]
  (let [endpoint-data {:path (get endpoint :path)
                       :method (get endpoint :method :get)
                       :summary (get endpoint :summary "")
                       :description (get endpoint :description "")
                       :parameters (get endpoint :parameters [])
                       :request-body (get endpoint :request-body)
                       :responses (get endpoint :responses {})
                       :tags (get endpoint :tags [])
                       :examples (get endpoint :examples [])}]
    (swap! state update-in [:api-specs spec-id :endpoints] conj endpoint-data)))

(defn generate-api-docs
  "Generate API documentation."
  [spec-id & {:keys [format] :or {format :markdown}}]
  (when-let [spec (get-api-spec spec-id)]
    (case format
      :markdown (generate-api-markdown spec)
      :html (generate-api-html spec)
      :openapi (generate-openapi spec)
      (generate-api-markdown spec))))

(defn- generate-api-markdown
  "Generate Markdown API documentation."
  [spec]
  (let [sb (StringBuilder.)]
    (.append sb (str "# " (:title spec) "\n\n"))
    (.append sb (str "Version: " (:version spec) "\n\n"))
    (.append sb (str "Base URL: `" (:base-url spec) "`\n\n"))
    
    (.append sb "## Endpoints\n\n")
    (doseq [endpoint (:endpoints spec)]
      (.append sb (str "### " (str/upper-case (name (:method endpoint))) " " (:path endpoint) "\n\n"))
      (.append sb (str (:summary endpoint) "\n\n"))
      (when (seq (:description endpoint))
        (.append sb (str (:description endpoint) "\n\n")))
      
      (when (seq (:parameters endpoint))
        (.append sb "**Parameters:**\n\n")
        (doseq [param (:parameters endpoint)]
          (.append sb (str "- `" (:name param) "` (" (:in param) "): " (:description param) "\n")))
        (.append sb "\n"))
      
      (when (seq (:responses endpoint))
        (.append sb "**Responses:**\n\n")
        (doseq [[code response] (:responses endpoint)]
          (.append sb (str "- `" code "`: " (:description response) "\n")))
        (.append sb "\n")))
    
    (.toString sb)))

(defn- generate-api-html
  "Generate HTML API documentation."
  [spec]
  (str "<html><head><title>" (:title spec) "</title></head><body>"
       "<h1>" (:title spec) "</h1>"
       "<p>Version: " (:version spec) "</p>"
       "</body></html>"))

(defn- generate-openapi
  "Generate OpenAPI specification."
  [spec]
  {:openapi "3.0.0"
   :info {:title (:title spec)
          :version (:version spec)}
   :servers [{:url (:base-url spec)}]
   :paths (reduce (fn [paths endpoint]
                    (assoc-in paths [(:path endpoint) (keyword (:method endpoint))]
                              {:summary (:summary endpoint)
                               :description (:description endpoint)
                               :parameters (:parameters endpoint)
                               :responses (:responses endpoint)}))
                  {}
                  (:endpoints spec))})

;; ============================================================================
;; Changelog Generation
;; ============================================================================

(defn create-changelog!
  "Create a changelog entry."
  [version config]
  (let [changelog {:version version
                   :date (get config :date (str (LocalDate/now)))
                   :added (get config :added [])
                   :changed (get config :changed [])
                   :deprecated (get config :deprecated [])
                   :removed (get config :removed [])
                   :fixed (get config :fixed [])
                   :security (get config :security [])
                   :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:changelogs version] changelog)
    (logging/log :info "Created changelog" {:version version})
    version))

(defn get-changelog
  "Get a changelog entry."
  [version]
  (get-in @state [:changelogs version]))

(defn list-changelogs
  "List all changelogs."
  []
  (sort-by :version > (vals (:changelogs @state))))

(defn generate-changelog-markdown
  "Generate changelog in Markdown format."
  []
  (let [changelogs (list-changelogs)
        sb (StringBuilder.)]
    (.append sb "# Changelog\n\n")
    (.append sb "All notable changes to this project will be documented in this file.\n\n")
    
    (doseq [cl changelogs]
      (.append sb (str "## [" (:version cl) "] - " (:date cl) "\n\n"))
      
      (when (seq (:added cl))
        (.append sb "### Added\n")
        (doseq [item (:added cl)]
          (.append sb (str "- " item "\n")))
        (.append sb "\n"))
      
      (when (seq (:changed cl))
        (.append sb "### Changed\n")
        (doseq [item (:changed cl)]
          (.append sb (str "- " item "\n")))
        (.append sb "\n"))
      
      (when (seq (:fixed cl))
        (.append sb "### Fixed\n")
        (doseq [item (:fixed cl)]
          (.append sb (str "- " item "\n")))
        (.append sb "\n"))
      
      (when (seq (:removed cl))
        (.append sb "### Removed\n")
        (doseq [item (:removed cl)]
          (.append sb (str "- " item "\n")))
        (.append sb "\n")))
    
    (.toString sb)))

;; ============================================================================
;; Diagram Generation
;; ============================================================================

(defn create-diagram!
  "Create a diagram."
  [diagram-id config]
  (let [diagram {:id diagram-id
                 :title (get config :title (name diagram-id))
                 :type (get config :type :flowchart) ;; :flowchart, :sequence, :class, :er, :state
                 :content (get config :content "")
                 :format (get config :format :mermaid) ;; :mermaid, :plantuml, :graphviz
                 :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:diagrams diagram-id] diagram)
    (logging/log :info "Created diagram" {:diagram-id diagram-id})
    diagram-id))

(defn get-diagram
  "Get a diagram."
  [diagram-id]
  (get-in @state [:diagrams diagram-id]))

(defn list-diagrams
  "List all diagrams."
  []
  (mapv (fn [[id d]]
          {:id id
           :title (:title d)
           :type (:type d)
           :format (:format d)})
        (:diagrams @state)))

(defn generate-architecture-diagram
  "Generate architecture diagram."
  []
  (create-diagram! :architecture
                   {:title "System Architecture"
                    :type :flowchart
                    :format :mermaid
                    :content "graph TB
    subgraph Client
        A[Web App] --> B[API Gateway]
        C[CLI] --> B
    end
    
    subgraph Core
        B --> D[Analysis Service]
        D --> E[LM Studio]
        D --> F[Model Registry]
    end
    
    subgraph Storage
        D --> G[(PostgreSQL)]
        D --> H[(Redis Cache)]
    end
    
    subgraph Monitoring
        I[Metrics] --> J[Grafana]
        K[Logs] --> L[Loki]
    end"}))

(defn generate-mental-model-diagram
  "Generate mental model relationship diagram."
  []
  (create-diagram! :mental-models
                   {:title "Mental Model Relationships"
                    :type :flowchart
                    :format :mermaid
                    :content "graph LR
    subgraph Cognitive Biases
        A[Confirmation Bias]
        B[Anchoring]
        C[Availability Heuristic]
    end
    
    subgraph Decision Making
        D[Incentives]
        E[Second-Order Thinking]
        F[Inversion]
    end
    
    A --> D
    B --> E
    C --> F
    
    subgraph Lollapalooza
        G[Combined Effect]
    end
    
    A --> G
    B --> G
    D --> G"}))

;; ============================================================================
;; Search Indexing
;; ============================================================================

(defn- tokenize
  "Tokenize text for indexing."
  [text]
  (when text
    (-> text
        str/lower-case
        (str/replace #"[^\w\s]" "")
        (str/split #"\s+")
        (->> (filter #(> (count %) 2))))))

(defn index-document!
  "Index a document for search."
  [doc-id]
  (when-let [doc (get-document doc-id)]
    (let [text (str (:title doc) " " (:description doc))
          tokens (tokenize text)]
      (doseq [token tokens]
        (swap! state update-in [:search-index token]
               (fnil conj #{})
               doc-id)))))

(defn search-documents
  "Search documents."
  [query]
  (let [tokens (tokenize query)
        results (reduce (fn [acc token]
                          (clojure.set/union acc (get-in @state [:search-index token] #{})))
                        #{}
                        tokens)]
    (mapv get-document results)))

;; ============================================================================
;; Export
;; ============================================================================

(defn export-documentation
  "Export all documentation."
  [& {:keys [format output-dir] :or {format :markdown output-dir "/tmp/docs"}}]
  (let [documents (vals (:documents @state))
        api-specs (vals (:api-specs @state))
        changelogs (list-changelogs)]
    {:documents (mapv (fn [doc]
                        {:id (:id doc)
                         :title (:title doc)
                         :content (render-document doc format)})
                      documents)
     :api-docs (mapv (fn [spec]
                       {:id (:id spec)
                        :content (generate-api-docs (:id spec) :format format)})
                     api-specs)
     :changelog (generate-changelog-markdown)
     :diagrams (mapv (fn [[id d]]
                       {:id id
                        :content (:content d)})
                     (:diagrams @state))}))

(defn- render-document
  "Render a document to specified format."
  [doc format]
  (case format
    :markdown (str "# " (:title doc) "\n\n" (:description doc))
    :html (str "<h1>" (:title doc) "</h1><p>" (:description doc) "</p>")
    (str (:title doc) "\n" (:description doc))))

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-documentation-stats
  "Get documentation statistics."
  []
  (let [stats (:stats @state)]
    {:total-documents (count (:documents @state))
     :total-sections (count (:sections @state))
     :total-api-specs (count (:api-specs @state))
     :total-changelogs (count (:changelogs @state))
     :total-diagrams (count (:diagrams @state))
     :search-index-terms (count (:search-index @state))
     :documents-generated (:documents-generated stats)
     :api-specs-generated (:api-specs-generated stats)}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-documentation-generator!
  "Initialize the documentation generator."
  []
  (when-not (:initialized? @state)
    ;; Create main documentation
    (create-document! :getting-started
                      {:title "Getting Started"
                       :description "Quick start guide for the Mental Models Analysis System"
                       :type :tutorial
                       :tags #{:beginner :setup}})
    
    (create-document! :api-reference
                      {:title "API Reference"
                       :description "Complete API reference documentation"
                       :type :api
                       :tags #{:api :reference}})
    
    (create-document! :mental-models-guide
                      {:title "Mental Models Guide"
                       :description "Guide to Charlie Munger's mental models"
                       :type :guide
                       :tags #{:mental-models :munger}})
    
    ;; Create API spec
    (create-api-spec! :main-api
                      {:title "Mental Models API"
                       :version "1.0.0"
                       :base-url "http://localhost:8080/api"})
    
    (add-endpoint! :main-api
                   {:path "/analyze"
                    :method :post
                    :summary "Analyze text for mental models"
                    :description "Analyzes the provided text and identifies mental models present"
                    :parameters [{:name "text" :in "body" :description "Text to analyze"}]
                    :responses {200 {:description "Analysis results"}
                                400 {:description "Invalid input"}}})
    
    (add-endpoint! :main-api
                   {:path "/models"
                    :method :get
                    :summary "List all mental models"
                    :description "Returns a list of all available mental models"
                    :responses {200 {:description "List of mental models"}}})
    
    ;; Create initial changelog
    (create-changelog! "1.0.0"
                       {:date "2024-01-01"
                        :added ["Initial release"
                                "Mental model analysis"
                                "LM Studio integration"
                                "Lollapalooza detection"]})
    
    ;; Generate diagrams
    (generate-architecture-diagram)
    (generate-mental-model-diagram)
    
    (swap! state assoc :initialized? true)
    (logging/log :info "Documentation generator initialized")
    (events/emit! :documentation-generator-initialized {})
    true))
