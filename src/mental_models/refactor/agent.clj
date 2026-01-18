(ns mental-models.refactor.agent
  "LLM-Powered Refactoring Agent
   Uses AI to automatically refactor tangled code for increased referential transparency"
  (:require [mental-models.refactor.dag :as dag]
            [mental-models.refactor.tangle :as tangle]
            [mental-models.services.lm-studio :as llm]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [taoensso.timbre :as log]))

;; -- Prompts -----------------------------------------------------------------

(def system-prompt
  "You are an expert Clojure refactoring agent. Your goal is to improve code quality by:
   
   1. INCREASING REFERENTIAL TRANSPARENCY
      - Replace side effects with pure functions where possible
      - Make dependencies explicit through function parameters
      - Eliminate hidden state and global mutable references
   
   2. SIMPLIFYING THE DEPENDENCY GRAPH
      - Break circular dependencies
      - Extract common functionality into shared modules
      - Reduce coupling between components
   
   3. PRESERVING BEHAVIOR
      - The refactored code MUST be semantically equivalent to the original
      - All public interfaces must remain unchanged
      - Tests must continue to pass
   
   When refactoring:
   - Prefer small, focused functions over large ones
   - Use descriptive names that reveal intent
   - Keep functions pure when possible
   - Use threading macros for clarity
   - Leverage Clojure idioms (destructuring, higher-order functions)
   
   Output format:
   - Provide the complete refactored code
   - Explain each change made
   - List any assumptions or caveats")

(defn build-refactor-prompt
  "Build a prompt for refactoring a specific tangle"
  [tangle-info code-snippets suggestions]
  (str "## Refactoring Task\n\n"
       "### Tangle Analysis\n"
       "- Nodes involved: " (count (:nodes tangle-info)) "\n"
       "- Density: " (format "%.2f" (:density tangle-info)) "\n"
       "- Complexity: " (:complexity tangle-info) "\n"
       "- Cohesion: " (format "%.2f" (:cohesion tangle-info)) "\n"
       "- Severity: " (name (:severity tangle-info)) "\n\n"
       "### Suggested Approaches\n"
       (str/join "\n" (map #(str "- " (:action %)) suggestions))
       "\n\n"
       "### Code to Refactor\n\n"
       "```clojure\n"
       code-snippets
       "\n```\n\n"
       "### Instructions\n"
       "1. Analyze the code structure and identify the root causes of tangling\n"
       "2. Propose a refactored version that:\n"
       "   - Reduces interconnections between functions\n"
       "   - Increases referential transparency\n"
       "   - Maintains the same external behavior\n"
       "3. Explain your changes\n\n"
       "Provide the refactored code in a ```clojure code block."))

;; -- Code Extraction ---------------------------------------------------------

(defn extract-code-for-nodes
  "Extract source code for the given node IDs"
  [node-ids dag]
  (let [node-set (set node-ids)
        nodes (filter #(node-set (:id %)) (:nodes dag))
        by-namespace (group-by :namespace nodes)]
    (for [[ns-name ns-nodes] by-namespace
          :let [file-node (first (filter #(= (:id %) (str ns-name)) (:nodes dag)))
                file-path (:path file-node)]
          :when file-path]
      {:namespace ns-name
       :path file-path
       :code (try (slurp file-path) (catch Exception _ nil))
       :definitions (map :name ns-nodes)})))

(defn extract-relevant-code
  "Extract only the relevant definitions from a file"
  [code definition-names]
  (let [forms (dag/read-all-forms (java.io.StringReader. code))
        relevant (filter (fn [form]
                          (and (seq? form)
                               (contains? #{'def 'defn 'defn- 'defmacro} (first form))
                               (contains? (set definition-names) (second form))))
                        forms)]
    (str/join "\n\n" (map #(with-out-str (pp/pprint %)) relevant))))

;; -- Refactoring Execution ---------------------------------------------------

(defn request-refactoring
  "Request refactoring from LLM"
  [tangle-info code-snippets suggestions]
  (let [prompt (build-refactor-prompt tangle-info code-snippets suggestions)
        response (llm/chat-completion
                  {:messages [{:role "system" :content system-prompt}
                              {:role "user" :content prompt}]
                   :temperature 0.2
                   :max-tokens 4096})]
    (if (:success response)
      {:success true
       :response (get-in response [:data :choices 0 :message :content])
       :usage (get-in response [:data :usage])}
      {:success false
       :error (:error response)})))

(defn parse-refactored-code
  "Parse refactored code from LLM response"
  [response]
  (let [code-blocks (re-seq #"```clojure\n([\s\S]*?)\n```" response)]
    (when (seq code-blocks)
      {:code (str/join "\n\n" (map second code-blocks))
       :explanation (str/replace response #"```clojure\n[\s\S]*?\n```" "[CODE BLOCK]")})))

;; -- Refactoring Pipeline ----------------------------------------------------

(defn refactor-tangle
  "Refactor a single tangle"
  [tangle dag]
  (log/info "Refactoring tangle with" (count (:nodes tangle)) "nodes")
  (let [code-info (extract-code-for-nodes (:nodes tangle) dag)
        code-snippets (str/join "\n\n;; ---\n\n"
                                (for [{:keys [namespace code definitions]} code-info
                                      :when code]
                                  (str ";; Namespace: " namespace "\n"
                                       (extract-relevant-code code definitions))))
        suggestions (get-in (tangle/suggest-refactoring tangle dag) [:suggestions])
        result (request-refactoring tangle code-snippets suggestions)]
    (if (:success result)
      (let [parsed (parse-refactored-code (:response result))]
        {:success true
         :original-tangle tangle
         :refactored-code (:code parsed)
         :explanation (:explanation parsed)
         :tokens-used (:usage result)})
      {:success false
       :error (:error result)
       :tangle tangle})))

;; -- Batch Refactoring -------------------------------------------------------

(defn refactor-top-tangles
  "Refactor the top N prioritized tangles"
  [dag & {:keys [n] :or {n 3}}]
  (let [analysis (tangle/detect-and-prioritize dag)
        top-tangles (take n (:prioritized-tangles analysis))]
    (log/info "Refactoring top" n "tangles")
    {:analysis analysis
     :refactorings (doall
                    (for [t top-tangles]
                      (do
                        (Thread/sleep 1000) ; Rate limiting
                        (refactor-tangle t dag))))}))

;; -- Code Application --------------------------------------------------------

(defn create-backup
  "Create backup of original file"
  [file-path]
  (let [backup-path (str file-path ".backup." (System/currentTimeMillis))]
    (io/copy (io/file file-path) (io/file backup-path))
    backup-path))

(defn apply-refactoring
  "Apply refactored code to file (with backup)"
  [file-path refactored-code & {:keys [dry-run] :or {dry-run true}}]
  (if dry-run
    {:action :dry-run
     :file file-path
     :would-write refactored-code}
    (let [backup (create-backup file-path)]
      (spit file-path refactored-code)
      {:action :applied
       :file file-path
       :backup backup})))

;; -- Verification Request ----------------------------------------------------

(defn request-verification-tests
  "Request LLM to generate verification tests for refactored code"
  [original-code refactored-code]
  (let [prompt (str "Generate property-based tests to verify that this refactored code "
                    "is semantically equivalent to the original.\n\n"
                    "Original:\n```clojure\n" original-code "\n```\n\n"
                    "Refactored:\n```clojure\n" refactored-code "\n```\n\n"
                    "Use clojure.test.check for property-based testing. "
                    "Focus on:\n"
                    "1. Same outputs for same inputs\n"
                    "2. Same side effects (if any)\n"
                    "3. Same error conditions\n"
                    "4. Edge cases")
        response (llm/chat-completion
                  {:messages [{:role "system" :content "You are an expert in Clojure testing and property-based testing."}
                              {:role "user" :content prompt}]
                   :temperature 0.2
                   :max-tokens 2048})]
    (if (:success response)
      (parse-refactored-code (get-in response [:data :choices 0 :message :content]))
      nil)))

;; -- Incremental Refactoring -------------------------------------------------

(defn suggest-incremental-changes
  "Suggest small, incremental changes rather than large refactorings"
  [tangle dag]
  (let [prompt (str "Analyze this tangled code and suggest 3-5 small, safe refactoring steps "
                    "that can be applied incrementally. Each step should:\n"
                    "1. Be independently verifiable\n"
                    "2. Not break existing functionality\n"
                    "3. Move toward better referential transparency\n\n"
                    "Code:\n```clojure\n"
                    (str/join "\n" (map pr-str (take 5 (:nodes tangle))))
                    "\n```")
        response (llm/simple-chat prompt
                                  :system-prompt "You are an expert in incremental refactoring."
                                  :temperature 0.3)]
    (when response
      {:tangle tangle
       :incremental-steps response})))

;; -- Main Entry Point --------------------------------------------------------

(defn autonomous-refactor
  "Main entry point for autonomous refactoring"
  [source-dir & {:keys [max-tangles dry-run]
                 :or {max-tangles 3 dry-run true}}]
  (log/info "Starting autonomous refactoring of" source-dir)
  (let [;; Step 1: Build DAG
        dag-analysis (dag/analyze-codebase source-dir)
        dag (:dag dag-analysis)
        
        ;; Step 2: Detect tangles
        tangle-analysis (tangle/detect-and-prioritize dag)
        
        ;; Step 3: Refactor top tangles
        refactorings (refactor-top-tangles dag :n max-tangles)]
    
    {:source-dir source-dir
     :dag-summary (:analysis dag-analysis)
     :tangle-summary (:summary (:analysis tangle-analysis))
     :refactorings (:refactorings refactorings)
     :dry-run dry-run
     :recommendations
     (for [{:keys [success original-tangle refactored-code explanation]} (:refactorings refactorings)
           :when success]
       {:tangle-id (first (:nodes original-tangle))
        :severity (:severity original-tangle)
        :action (if dry-run "Review suggested changes" "Changes applied")
        :explanation (take 500 explanation)})}))
