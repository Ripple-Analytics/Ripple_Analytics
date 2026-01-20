(ns mental-models.main.section-5
  "Main Module - Section 5"
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [ring.adapter.jetty :as jetty])
            #?(:clj [cheshire.core :as json])
            [mental-models.models :as models]
            [mental-models.algorithms :as algo]))

;; ============================================

(defn json-response [data]
  (-> (response/response (cheshire.core/generate-string data))
      (response/content-type "application/json")))

(defn handle-get-models [request]
  (json-response (models/export-all-models)))

(defn handle-latticework [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")
        model-names (get body :models [])]
    (json-response (analysis/latticework-analyze model-names context))))

(defn handle-lollapalooza [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")
        model-names (get body :models [])]
    (json-response (analysis/detect-lollapalooza model-names context))))

(defn handle-inversion [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        problem (get body :problem "")]
    (json-response (analysis/invert problem))))

(defn handle-two-track [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        situation (get body :situation "")]
    (json-response (analysis/two-track-analysis situation))))

(defn handle-bias-detection [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (analysis/detect-biases text))))

(defn handle-correlation [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        xs (get body :x [])
        ys (get body :y [])]
    (json-response (stats/correlation-analysis xs ys))))

(defn handle-document-analysis [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (data/analyze-document text))))

;; LLM-powered handlers
(defn handle-llm-analyze [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        situation (get body :situation "")
        model-names (get body :models [])]
    (json-response (analyze-with-llm situation model-names))))

(defn handle-llm-biases [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (detect-biases-with-llm text))))

(defn handle-llm-checklist [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        context (get body :context "")]
    (json-response (generate-decision-checklist-with-llm context))))

(defn handle-llm-classify [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        text (get body :text "")]
    (json-response (classify-document-with-llm text))))

(defn handle-llm-status [request]
  (json-response {:lm_studio_url (:base-url lm-studio-config)
                  :model (:model lm-studio-config)
                  :status (if (call-lm-studio "test" :max-tokens 5) "connected" "disconnected")}))

;; Tech Debt handlers
(defn handle-analyze-dag [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/analyze-codebase dag)))))

(defn handle-detect-tangles [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/detect-tangles dag)))))

(defn handle-refactoring-plan [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)
          analysis (tech-debt/analyze-codebase dag)]
      (json-response (tech-debt/generate-refactoring-plan analysis)))))

(defn handle-llm-refactor [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        tangle (get body :tangle {})
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)
          prompt (tech-debt/generate-llm-refactoring-prompt tangle dag)]
      (if-let [response (call-lm-studio prompt)]
        (json-response {:success true :refactoring-suggestion response})
        (json-response {:success false :error "LM Studio not available"
                       :prompt prompt})))))

(defn handle-dag-visualization [request]
  (let [body (cheshire.core/parse-string (slurp (:body request)) true)
        dag-data (get body :dag {:nodes {} :edges []})]
    (let [dag (tech-debt/build-dag-from-code dag-data)]
      (json-response (tech-debt/export-dag-for-visualization dag)))))

