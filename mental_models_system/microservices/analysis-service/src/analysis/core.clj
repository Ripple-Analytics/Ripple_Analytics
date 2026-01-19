(ns analysis.core
  "Analysis Service
   
   Mental model detection and analysis microservice.
   Provides keyword-based and LLM-powered analysis.
   Hot-loadable for continuous deployment."
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:gen-class))

;; ============================================
;; Mental Models Database
;; ============================================

(def mental-models
  "Core mental models database"
  [{:id 1
    :name "Incentive-Caused Bias"
    :category "Psychology"
    :keywords ["incentive" "reward" "punishment" "motivation" "bonus" "commission" "salary" "pay"]
    :description "People tend to act in ways that serve their incentives, even unconsciously."
    :failure-modes ["Incentive Blindness" "Perverse Incentive Creation"]
    :safeguards ["Consider non-monetary incentives" "Map conflicts of interest"]}
   
   {:id 2
    :name "Confirmation Bias"
    :category "Psychology"
    :keywords ["confirm" "belief" "evidence" "ignore" "dismiss" "agree" "disagree" "opinion"]
    :description "The tendency to search for, interpret, and recall information that confirms pre-existing beliefs."
    :failure-modes ["Echo Chamber Effect" "Selective Evidence Gathering"]
    :safeguards ["Actively seek disconfirming evidence" "Devil's advocate role"]}
   
   {:id 3
    :name "Availability Heuristic"
    :category "Psychology"
    :keywords ["recent" "memorable" "vivid" "news" "media" "remember" "recall" "example"]
    :description "Overweighting information that comes easily to mind."
    :failure-modes ["Recency Bias" "Media Influence Distortion"]
    :safeguards ["Use base rates" "Seek statistical data"]}
   
   {:id 4
    :name "Sunk Cost Fallacy"
    :category "Economics"
    :keywords ["sunk" "cost" "invested" "spent" "waste" "continue" "quit" "abandon"]
    :description "Continuing a behavior due to previously invested resources rather than future value."
    :failure-modes ["Escalation of Commitment" "Loss Aversion Trap"]
    :safeguards ["Focus on future value only" "Set kill criteria upfront"]}
   
   {:id 5
    :name "Opportunity Cost"
    :category "Economics"
    :keywords ["opportunity" "alternative" "trade-off" "choice" "option" "instead" "rather"]
    :description "The cost of the next best alternative foregone."
    :failure-modes ["Hidden Cost Blindness" "Comparison Neglect"]
    :safeguards ["Always list alternatives" "Quantify trade-offs"]}
   
   {:id 6
    :name "Margin of Safety"
    :category "Engineering"
    :keywords ["margin" "safety" "buffer" "redundancy" "backup" "cushion" "reserve"]
    :description "Building in a buffer to account for errors, unknowns, and variability."
    :failure-modes ["Overconfidence in Estimates" "Insufficient Buffer"]
    :safeguards ["Add 50% buffer minimum" "Plan for worst case"]}
   
   {:id 7
    :name "Feedback Loops"
    :category "Systems"
    :keywords ["feedback" "loop" "cycle" "reinforce" "amplify" "dampen" "stabilize"]
    :description "Systems where outputs become inputs, creating reinforcing or balancing dynamics."
    :failure-modes ["Runaway Amplification" "Delayed Feedback Blindness"]
    :safeguards ["Map feedback mechanisms" "Monitor leading indicators"]}
   
   {:id 8
    :name "Second-Order Thinking"
    :category "Thinking Tools"
    :keywords ["consequence" "effect" "result" "then what" "downstream" "ripple" "chain"]
    :description "Considering the consequences of consequences."
    :failure-modes ["First-Order Fixation" "Unintended Consequence Blindness"]
    :safeguards ["Ask 'and then what?' 3 times" "Map causal chains"]}
   
   {:id 9
    :name "Inversion"
    :category "Thinking Tools"
    :keywords ["invert" "reverse" "opposite" "avoid" "prevent" "failure" "mistake"]
    :description "Approaching problems backwards - thinking about what to avoid rather than what to do."
    :failure-modes ["Positive-Only Thinking" "Failure Mode Neglect"]
    :safeguards ["Pre-mortem exercise" "List ways to fail"]}
   
   {:id 10
    :name "Circle of Competence"
    :category "Thinking Tools"
    :keywords ["competence" "expertise" "knowledge" "understand" "limit" "boundary" "edge"]
    :description "Understanding the boundaries of your knowledge and staying within them."
    :failure-modes ["Overconfidence Outside Circle" "Circle Creep"]
    :safeguards ["Define circle explicitly" "Seek expert input outside circle"]}
   
   {:id 11
    :name "First Principles"
    :category "Thinking Tools"
    :keywords ["first principle" "fundamental" "basic" "assumption" "ground up" "foundation"]
    :description "Breaking down problems to their most basic elements and building up from there."
    :failure-modes ["Assumption Inheritance" "Complexity Avoidance"]
    :safeguards ["Question every assumption" "Start from physics/math"]}
   
   {:id 12
    :name "Occam's Razor"
    :category "Thinking Tools"
    :keywords ["simple" "simplest" "complexity" "complicated" "explanation" "assumption"]
    :description "The simplest explanation is usually the correct one."
    :failure-modes ["Oversimplification" "Complexity Bias"]
    :safeguards ["Start simple, add complexity only when needed"]}
   
   {:id 13
    :name "Hanlon's Razor"
    :category "Psychology"
    :keywords ["malice" "stupidity" "incompetence" "mistake" "intention" "deliberate"]
    :description "Never attribute to malice that which can be explained by incompetence."
    :failure-modes ["Paranoid Attribution" "Malice Assumption"]
    :safeguards ["Assume good intent first" "Consider alternative explanations"]}
   
   {:id 14
    :name "Network Effects"
    :category "Economics"
    :keywords ["network" "user" "platform" "value" "growth" "viral" "adoption"]
    :description "The value of a product increases as more people use it."
    :failure-modes ["Network Dependency Risk" "Winner-Take-All Blindness"]
    :safeguards ["Diversify platforms" "Build owned audience"]}
   
   {:id 15
    :name "Compounding"
    :category "Mathematics"
    :keywords ["compound" "exponential" "growth" "interest" "accumulate" "snowball"]
    :description "Small consistent gains accumulate into large results over time."
    :failure-modes ["Linear Thinking" "Impatience with Growth"]
    :safeguards ["Think in decades" "Protect the principal"]}])

;; ============================================
;; Analysis Functions
;; ============================================

(defn analyze-text
  "Analyze text for mental model patterns using keyword matching"
  [text top-n]
  (let [lower-text (str/lower-case text)
        results (for [model mental-models
                      :let [matched-keywords (filter #(str/includes? lower-text (str/lower-case %))
                                                     (:keywords model))
                            match-count (count matched-keywords)]
                      :when (pos? match-count)]
                  {:id (:id model)
                   :name (:name model)
                   :category (:category model)
                   :description (:description model)
                   :relevance (min 1.0 (* 2.0 (/ match-count (count (:keywords model)))))
                   :matched-keywords matched-keywords
                   :failure-modes (:failure-modes model)
                   :safeguards (:safeguards model)})]
    (->> results
         (sort-by :relevance >)
         (take top-n)
         vec)))

(defn get-all-models
  "Get all mental models"
  []
  (map #(select-keys % [:id :name :category :description]) mental-models))

(defn get-model-by-id
  "Get a specific mental model by ID"
  [id]
  (first (filter #(= (:id %) id) mental-models)))

(defn get-all-categories
  "Get all unique categories"
  []
  (->> mental-models
       (map :category)
       distinct
       sort
       vec))

(defn get-models-by-category
  "Get models by category"
  [category]
  (filter #(= (str/lower-case (:category %)) (str/lower-case category)) mental-models))

;; ============================================
;; LLM Integration (Optional)
;; ============================================

(def lm-studio-url
  "LM Studio URL for AI-powered analysis"
  (or (System/getenv "LM_STUDIO_URL") "http://localhost:1234"))

(defn call-llm
  "Call LM Studio for AI-powered analysis (optional)"
  [prompt]
  (try
    (let [response (http/post (str lm-studio-url "/v1/chat/completions")
                              {:content-type :json
                               :body (json/generate-string
                                      {:model "local-model"
                                       :messages [{:role "system"
                                                   :content "You are a mental models analyst."}
                                                  {:role "user"
                                                   :content prompt}]
                                       :max_tokens 1000
                                       :temperature 0.7})
                               :socket-timeout 60000
                               :connection-timeout 5000
                               :throw-exceptions false})]
      (when (= 200 (:status response))
        (get-in (json/parse-string (:body response) true) [:choices 0 :message :content])))
    (catch Exception e
      (log/warn "LLM call failed:" (.getMessage e))
      nil)))

;; ============================================
;; Configuration
;; ============================================

(def service-port
  "Port for the Analysis Service"
  (Integer/parseInt (or (System/getenv "PORT") "8001")))

;; ============================================
;; Route Handlers
;; ============================================

(defn root-handler
  "Root endpoint"
  [_]
  (response/response
   {:service "Mental Models Analysis Service"
    :version "1.0.0"
    :models-count (count mental-models)}))

(defn health-handler
  "Health check endpoint"
  [_]
  (response/response {:status "healthy" :service "analysis"}))

(defn analyze-handler
  "Analyze text for mental models"
  [request]
  (let [body (:body request)
        text (or (:text body) "")
        top-n (or (:top_n body) (:top-n body) 5)]
    (if (str/blank? text)
      (-> (response/response {:error "No text provided for analysis"})
          (response/status 400))
      (let [results (analyze-text text top-n)]
        (response/response
         {:success true
          :mode "keyword"
          :text-preview (str (subs text 0 (min 100 (count text)))
                             (when (> (count text) 100) "..."))
          :models results
          :total-models-checked (count mental-models)
          :timestamp (str (java.time.Instant/now))})))))

(defn models-handler
  "Get all mental models"
  [_]
  (response/response
   {:models (get-all-models)
    :count (count mental-models)}))

(defn model-by-id-handler
  "Get a specific mental model"
  [request]
  (let [id-str (get-in request [:params :id])
        id (try (Integer/parseInt id-str) (catch Exception _ nil))]
    (if-let [model (and id (get-model-by-id id))]
      (response/response model)
      (-> (response/response {:error "Model not found"})
          (response/status 404)))))

(defn categories-handler
  "Get all categories"
  [_]
  (response/response
   {:categories (get-all-categories)
    :count (count (get-all-categories))}))

(defn category-models-handler
  "Get models by category"
  [request]
  (let [category (get-in request [:params :category])
        models (get-models-by-category category)]
    (if (seq models)
      (response/response
       {:category category
        :models models
        :count (count models)})
      (-> (response/response {:error "Category not found"})
          (response/status 404)))))

;; ============================================
;; Router
;; ============================================

(defn router
  "Simple path-based router"
  [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (= uri "/") (root-handler request)
      (= uri "/health") (health-handler request)
      (and (= uri "/analyze") (= method :post)) (analyze-handler request)
      (= uri "/models") (models-handler request)
      (re-matches #"/models/(\d+)" uri)
      (let [id (second (re-matches #"/models/(\d+)" uri))]
        (model-by-id-handler (assoc-in request [:params :id] id)))
      (= uri "/categories") (categories-handler request)
      (re-matches #"/categories/(.+)" uri)
      (let [category (second (re-matches #"/categories/(.+)" uri))]
        (category-models-handler (assoc-in request [:params :category] category)))
      :else (-> (response/response {:error "Not found"})
                (response/status 404)))))

;; ============================================
;; Middleware
;; ============================================

(defn wrap-cors
  "Add CORS headers"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
          (response/header "Access-Control-Allow-Origin" "*")
          (response/header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS")
          (response/header "Access-Control-Allow-Headers" "Content-Type, Authorization")))))

(defn wrap-logging
  "Log requests"
  [handler]
  (fn [request]
    (log/info "Request:" (:request-method request) (:uri request))
    (let [response (handler request)]
      (log/info "Response:" (:status response))
      response)))

(def app
  "Main application handler"
  (-> router
      wrap-cors
      wrap-logging
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-defaults api-defaults)))

;; ============================================
;; Server
;; ============================================

(defn start-server
  "Start the Analysis Service server"
  []
  (log/info "Starting Analysis Service on port" service-port)
  (jetty/run-jetty app {:port service-port :join? false}))

(defn -main
  "Main entry point"
  [& _args]
  (log/info "Mental Models Analysis Service starting...")
  (start-server)
  (log/info "Analysis Service ready on port" service-port))
