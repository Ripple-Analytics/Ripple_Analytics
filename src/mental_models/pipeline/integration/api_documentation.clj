(ns mental-models.pipeline.integration.api-documentation
  "API documentation generator for mental model analysis system.
   
   Features:
   - OpenAPI/Swagger generation
   - Route documentation
   - Schema documentation
   - Example generation
   - Interactive documentation
   - Markdown export
   - Versioned documentation
   - Authentication documentation"
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
  (atom {:endpoints {}        ;; endpoint-id -> endpoint-doc
         :schemas {}          ;; schema-id -> schema-doc
         :tags {}             ;; tag-id -> tag-doc
         :security-schemes {} ;; scheme-id -> security-scheme
         :info {:title "Mental Models Analysis API"
                :description "API for analyzing text using Charlie Munger's mental models"
                :version "1.0.0"
                :contact {:name "API Support"
                          :email "support@example.com"}
                :license {:name "MIT"
                          :url "https://opensource.org/licenses/MIT"}}
         :servers [{:url "http://localhost:8080"
                    :description "Development server"}]
         :stats {:endpoints-documented 0 :schemas-documented 0}
         :initialized? false}))

;; ============================================================================
;; Endpoint Documentation
;; ============================================================================

(defn document-endpoint!
  "Document an API endpoint."
  [endpoint-id config]
  (let [endpoint {:id endpoint-id
                  :path (get config :path)
                  :method (get config :method :get)
                  :summary (get config :summary "")
                  :description (get config :description "")
                  :tags (get config :tags [])
                  :parameters (get config :parameters [])
                  :request-body (get config :request-body)
                  :responses (get config :responses {})
                  :security (get config :security [])
                  :deprecated? (get config :deprecated? false)
                  :examples (get config :examples {})
                  :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:endpoints endpoint-id] endpoint)
    (swap! state update-in [:stats :endpoints-documented] inc)
    (logging/log :debug "Documented endpoint" {:endpoint-id endpoint-id :path (:path endpoint)})
    endpoint-id))

(defn get-endpoint-doc
  "Get endpoint documentation."
  [endpoint-id]
  (get-in @state [:endpoints endpoint-id]))

(defn list-endpoints
  "List all documented endpoints."
  [& {:keys [tag method]}]
  (let [endpoints (vals (:endpoints @state))
        filtered (cond->> endpoints
                   tag (filter #(contains? (set (:tags %)) tag))
                   method (filter #(= (:method %) method)))]
    (mapv #(select-keys % [:id :path :method :summary :tags]) filtered)))

(defn update-endpoint!
  "Update endpoint documentation."
  [endpoint-id updates]
  (swap! state update-in [:endpoints endpoint-id] merge updates))

(defn deprecate-endpoint!
  "Mark an endpoint as deprecated."
  [endpoint-id & {:keys [message replacement]}]
  (swap! state update-in [:endpoints endpoint-id]
         assoc :deprecated? true
         :deprecation-message message
         :replacement replacement))

;; ============================================================================
;; Schema Documentation
;; ============================================================================

(defn document-schema!
  "Document a data schema."
  [schema-id config]
  (let [schema {:id schema-id
                :name (get config :name (name schema-id))
                :description (get config :description "")
                :type (get config :type :object)
                :properties (get config :properties {})
                :required (get config :required [])
                :example (get config :example)
                :created-at (System/currentTimeMillis)}]
    (swap! state assoc-in [:schemas schema-id] schema)
    (swap! state update-in [:stats :schemas-documented] inc)
    (logging/log :debug "Documented schema" {:schema-id schema-id})
    schema-id))

(defn get-schema-doc
  "Get schema documentation."
  [schema-id]
  (get-in @state [:schemas schema-id]))

(defn list-schemas
  "List all documented schemas."
  []
  (mapv (fn [[id s]]
          {:id id
           :name (:name s)
           :type (:type s)})
        (:schemas @state)))

;; ============================================================================
;; Tag Documentation
;; ============================================================================

(defn document-tag!
  "Document an API tag."
  [tag-id config]
  (let [tag {:id tag-id
             :name (get config :name (name tag-id))
             :description (get config :description "")
             :external-docs (get config :external-docs)}]
    (swap! state assoc-in [:tags tag-id] tag)
    tag-id))

(defn get-tag-doc
  "Get tag documentation."
  [tag-id]
  (get-in @state [:tags tag-id]))

(defn list-tags
  "List all documented tags."
  []
  (mapv (fn [[id t]]
          {:id id
           :name (:name t)
           :description (:description t)})
        (:tags @state)))

;; ============================================================================
;; Security Scheme Documentation
;; ============================================================================

(defn document-security-scheme!
  "Document a security scheme."
  [scheme-id config]
  (let [scheme {:id scheme-id
                :type (get config :type :http) ;; :http, :apiKey, :oauth2, :openIdConnect
                :scheme (get config :scheme "bearer")
                :bearer-format (get config :bearer-format "JWT")
                :name (get config :name)
                :in (get config :in) ;; :header, :query, :cookie
                :description (get config :description "")}]
    (swap! state assoc-in [:security-schemes scheme-id] scheme)
    scheme-id))

(defn get-security-scheme
  "Get security scheme documentation."
  [scheme-id]
  (get-in @state [:security-schemes scheme-id]))

;; ============================================================================
;; OpenAPI Generation
;; ============================================================================

(defn- convert-parameter
  "Convert parameter to OpenAPI format."
  [param]
  {:name (:name param)
   :in (name (:in param :query))
   :description (:description param "")
   :required (:required? param false)
   :schema {:type (name (:type param :string))}})

(defn- convert-response
  "Convert response to OpenAPI format."
  [status response]
  {status {:description (:description response "")
           :content (when (:schema response)
                      {"application/json"
                       {:schema {:$ref (str "#/components/schemas/" (name (:schema response)))}}})}})

(defn- convert-endpoint
  "Convert endpoint to OpenAPI path item."
  [endpoint]
  {(keyword (name (:method endpoint)))
   {:summary (:summary endpoint)
    :description (:description endpoint)
    :tags (:tags endpoint)
    :parameters (mapv convert-parameter (:parameters endpoint))
    :requestBody (when (:request-body endpoint)
                   {:required true
                    :content {"application/json"
                              {:schema {:$ref (str "#/components/schemas/" (name (:request-body endpoint)))}}}})
    :responses (reduce-kv (fn [m status resp]
                            (merge m (convert-response status resp)))
                          {}
                          (:responses endpoint))
    :security (when (seq (:security endpoint))
                (mapv (fn [s] {(name s) []}) (:security endpoint)))
    :deprecated (:deprecated? endpoint false)}})

(defn- convert-schema
  "Convert schema to OpenAPI format."
  [schema]
  {:type (name (:type schema))
   :description (:description schema)
   :properties (reduce-kv (fn [m k v]
                            (assoc m k {:type (name (:type v :string))
                                        :description (:description v "")}))
                          {}
                          (:properties schema))
   :required (:required schema)
   :example (:example schema)})

(defn generate-openapi
  "Generate OpenAPI specification."
  []
  (let [info (:info @state)
        servers (:servers @state)
        endpoints (:endpoints @state)
        schemas (:schemas @state)
        tags (:tags @state)
        security-schemes (:security-schemes @state)
        
        ;; Group endpoints by path
        paths (reduce (fn [paths [_ endpoint]]
                        (update paths (:path endpoint)
                                merge (convert-endpoint endpoint)))
                      {}
                      endpoints)]
    
    {:openapi "3.0.3"
     :info info
     :servers servers
     :tags (mapv (fn [[_ t]]
                   {:name (:name t)
                    :description (:description t)})
                 tags)
     :paths paths
     :components {:schemas (reduce-kv (fn [m id schema]
                                        (assoc m (name id) (convert-schema schema)))
                                      {}
                                      schemas)
                  :securitySchemes (reduce-kv (fn [m id scheme]
                                                (assoc m (name id)
                                                       {:type (name (:type scheme))
                                                        :scheme (:scheme scheme)
                                                        :bearerFormat (:bearer-format scheme)}))
                                              {}
                                              security-schemes)}}))

;; ============================================================================
;; Markdown Generation
;; ============================================================================

(defn generate-markdown
  "Generate Markdown documentation."
  []
  (let [sb (StringBuilder.)
        info (:info @state)
        endpoints (sort-by :path (vals (:endpoints @state)))
        schemas (vals (:schemas @state))]
    
    ;; Title and description
    (.append sb (str "# " (:title info) "\n\n"))
    (.append sb (str (:description info) "\n\n"))
    (.append sb (str "**Version:** " (:version info) "\n\n"))
    
    ;; Table of contents
    (.append sb "## Table of Contents\n\n")
    (.append sb "- [Endpoints](#endpoints)\n")
    (.append sb "- [Schemas](#schemas)\n\n")
    
    ;; Endpoints
    (.append sb "## Endpoints\n\n")
    (doseq [endpoint endpoints]
      (.append sb (str "### " (str/upper-case (name (:method endpoint))) " " (:path endpoint) "\n\n"))
      (.append sb (str (:summary endpoint) "\n\n"))
      (when (seq (:description endpoint))
        (.append sb (str (:description endpoint) "\n\n")))
      (when (:deprecated? endpoint)
        (.append sb "**DEPRECATED**\n\n"))
      
      ;; Parameters
      (when (seq (:parameters endpoint))
        (.append sb "**Parameters:**\n\n")
        (.append sb "| Name | In | Type | Required | Description |\n")
        (.append sb "|------|-----|------|----------|-------------|\n")
        (doseq [param (:parameters endpoint)]
          (.append sb (str "| " (:name param) " | " (name (:in param)) " | "
                           (name (:type param :string)) " | "
                           (if (:required? param) "Yes" "No") " | "
                           (:description param "") " |\n")))
        (.append sb "\n"))
      
      ;; Responses
      (when (seq (:responses endpoint))
        (.append sb "**Responses:**\n\n")
        (doseq [[status response] (:responses endpoint)]
          (.append sb (str "- **" status "**: " (:description response) "\n")))
        (.append sb "\n")))
    
    ;; Schemas
    (.append sb "## Schemas\n\n")
    (doseq [schema schemas]
      (.append sb (str "### " (:name schema) "\n\n"))
      (.append sb (str (:description schema) "\n\n"))
      (when (seq (:properties schema))
        (.append sb "**Properties:**\n\n")
        (.append sb "| Name | Type | Description |\n")
        (.append sb "|------|------|-------------|\n")
        (doseq [[name prop] (:properties schema)]
          (.append sb (str "| " (clojure.core/name name) " | "
                           (clojure.core/name (:type prop :string)) " | "
                           (:description prop "") " |\n")))
        (.append sb "\n")))
    
    (.toString sb)))

;; ============================================================================
;; Ring Handler
;; ============================================================================

(defn openapi-handler
  "Ring handler for OpenAPI spec."
  [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (pr-str (generate-openapi))})

(defn docs-handler
  "Ring handler for Markdown docs."
  [request]
  {:status 200
   :headers {"Content-Type" "text/markdown"}
   :body (generate-markdown)})

;; ============================================================================
;; Statistics
;; ============================================================================

(defn get-documentation-stats
  "Get documentation statistics."
  []
  (let [stats (:stats @state)]
    {:endpoints-documented (:endpoints-documented stats)
     :schemas-documented (:schemas-documented stats)
     :tags-count (count (:tags @state))
     :security-schemes-count (count (:security-schemes @state))}))

;; ============================================================================
;; Initialization
;; ============================================================================

(defn init-api-documentation!
  "Initialize the API documentation system."
  []
  (when-not (:initialized? @state)
    ;; Document tags
    (document-tag! :analysis
                   {:name "Analysis"
                    :description "Mental model analysis endpoints"})
    
    (document-tag! :models
                   {:name "Models"
                    :description "Mental model management endpoints"})
    
    (document-tag! :health
                   {:name "Health"
                    :description "Health check endpoints"})
    
    ;; Document security schemes
    (document-security-scheme! :bearer-auth
                               {:type :http
                                :scheme "bearer"
                                :bearer-format "JWT"
                                :description "JWT Bearer token authentication"})
    
    ;; Document common schemas
    (document-schema! :analysis-request
                      {:name "AnalysisRequest"
                       :description "Request to analyze text"
                       :type :object
                       :properties {:text {:type :string :description "Text to analyze"}
                                    :options {:type :object :description "Analysis options"}}
                       :required [:text]})
    
    (document-schema! :analysis-response
                      {:name "AnalysisResponse"
                       :description "Analysis results"
                       :type :object
                       :properties {:id {:type :string :description "Analysis ID"}
                                    :models {:type :array :description "Detected mental models"}
                                    :lollapalooza {:type :boolean :description "Lollapalooza effect detected"}}})
    
    (document-schema! :mental-model
                      {:name "MentalModel"
                       :description "A mental model"
                       :type :object
                       :properties {:id {:type :string :description "Model ID"}
                                    :name {:type :string :description "Model name"}
                                    :category {:type :string :description "Model category"}
                                    :description {:type :string :description "Model description"}}})
    
    ;; Document endpoints
    (document-endpoint! :analyze-text
                        {:path "/api/v1/analyze"
                         :method :post
                         :summary "Analyze text for mental models"
                         :description "Analyzes the provided text and identifies relevant mental models"
                         :tags ["Analysis"]
                         :request-body :analysis-request
                         :responses {200 {:description "Analysis results"
                                          :schema :analysis-response}
                                     400 {:description "Invalid request"}
                                     500 {:description "Server error"}}
                         :security [:bearer-auth]})
    
    (document-endpoint! :list-models
                        {:path "/api/v1/models"
                         :method :get
                         :summary "List all mental models"
                         :description "Returns a list of all available mental models"
                         :tags ["Models"]
                         :parameters [{:name "category" :in :query :type :string
                                       :description "Filter by category"}]
                         :responses {200 {:description "List of mental models"}}})
    
    (document-endpoint! :health-check
                        {:path "/health"
                         :method :get
                         :summary "Health check"
                         :description "Returns the health status of the service"
                         :tags ["Health"]
                         :responses {200 {:description "Service is healthy"}
                                     503 {:description "Service is unhealthy"}}})
    
    (swap! state assoc :initialized? true)
    (logging/log :info "API documentation initialized")
    (events/emit! :api-documentation-initialized {})
    true))
